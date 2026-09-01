// The stage every variant of the capture redesign is judged on: ONE table,
// ONE fake template registry, ONE draft model, ONE set of editors, ONE pair of
// commit/cancel laws.  A variant brings a `look' and its own CSS and nothing
// else, so what differs between two tabs is HOW THE CAPTURE DOC IS REACHED AND
// WHERE IT STANDS — never what it knows.
//
// A CLASSIC SCRIPT ON PURPOSE: a module over `file://' is an opaque origin and
// will not load, and every page here must open by double-clicking it.
"use strict";

var RIG = (function () {

  // ================================================================== clock
  // UTC throughout: a local-midnight `Date' shifts a day across a DST boundary,
  // and a draft whose SCHEDULED slot moved with the reader's timezone would be
  // a different draft in Berlin.  ONE CLOCK READ, at mount (`docs/invariants.md',
  // "One clock read per request").
  var DAY = 86400000;
  function dnum(c) { return Math.round(Date.UTC(c.y, c.m - 1, c.d) / DAY); }
  function civil(n) {
    var t = new Date(n * DAY);
    return { y: t.getUTCFullYear(), m: t.getUTCMonth() + 1, d: t.getUTCDate() };
  }
  function addDays(c, n) { return civil(dnum(c) + n); }
  var DOW = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
  // THE WEEKDAY IS COMPUTED, never carried (`AGENTS.hs:213'): a stamp cannot
  // disagree with its own date.
  function dow(c) { return DOW[new Date(dnum(c) * DAY).getUTCDay()]; }
  function pad2(n) { return (n < 10 ? "0" : "") + n; }
  function iso(c) { return c.y + "-" + pad2(c.m) + "-" + pad2(c.d); }
  function stampOf(c) { return "<" + iso(c) + " " + dow(c) + ">"; }
  function inactive(c, hh, mm) {
    return "[" + iso(c) + " " + dow(c) + " " + pad2(hh) + ":" + pad2(mm) + "]";
  }

  // =============================================================== resolver
  // THE DATE GRAMMAR IS FAKED HERE and faked small.  The real one is the
  // date-widget spike's (`spikes/2026-08-23-date-widget/rig.js'), which
  // shipped as `#ddate'/`#dwhen'/`.dgh' (`Web/Page/Style.hs'); this spike is
  // about the DOC around it, so it carries a few canned phrases and the shape
  // of the answer rather than the whole grammar.
  var CANNED = {
    today: 0, tomorrow: 1, "+1d": 1, "+2d": 2, "+3d": 3, "+1w": 7, "+2w": 14,
    "next week": 7, "next month": 30,
  };
  var WEEKDAYS = { mon: 1, tue: 2, wed: 3, thu: 4, fri: 5, sat: 6, sun: 0 };
  var DATE_VOCAB = ["today", "tomorrow", "+1d", "+3d", "+1w", "+2w",
                    "next week", "mon", "fri"];

  /** TEXT read as a date, against TODAY.  Three answers and no fourth — the
   * date widget's own law: it RESOLVES, it is still being WRITTEN (and the
   * ghost keeps quiet), or the grammar REFUSES it outright. */
  function readDate(text) {
    var t = String(text == null ? "" : text).trim().toLowerCase();
    if (!t) return { clear: true };
    if (Object.prototype.hasOwnProperty.call(CANNED, t))
      return { ok: true, at: addDays(state.today, CANNED[t]) };
    if (Object.prototype.hasOwnProperty.call(WEEKDAYS, t)) {
      var want = WEEKDAYS[t], have = new Date(dnum(state.today) * DAY).getUTCDay();
      return { ok: true, at: addDays(state.today, ((want - have) + 7) % 7 || 7) };
    }
    var m = /^(\d{4})-(\d{1,2})-(\d{1,2})$/.exec(t);
    if (m) {
      var c = { y: +m[1], m: +m[2], d: +m[3] };
      if (c.m < 1 || c.m > 12 || c.d < 1 || c.d > 31)
        return { bad: "no such day on the calendar", hard: true };
      return { ok: true, at: c };
    }
    // ORG'S OWN SPELLING, KEPT VERBATIM once it reparses (`AGENTS.hs:3426').
    var v = /^<(\d{4})-(\d{1,2})-(\d{1,2})[^<>]*>$/.exec(t);
    if (v) return { ok: true, verbatim: String(text).trim() };
    // A TERM STILL BEING WRITTEN IS NOT A MISTAKE: a refusal flashed at every
    // keystroke is a refusal nobody reads.
    if (writing(t)) return { writing: true };
    return { bad: "no date here — try today, +1w, fri or 2026-09-01" };
  }
  function writing(t) {
    if (/^\d{0,4}(-\d{0,2}(-\d{0,2})?)?$/.test(t)) return true;
    if (/^[+-]\d*[dwmy]?$/.test(t)) return true;
    return DATE_VOCAB.concat(Object.keys(WEEKDAYS))
      .some(function (w) { return w.indexOf(t) === 0; });
  }
  function dateStamp(r) {
    if (!r || !r.ok) return "";
    return r.verbatim ? r.verbatim : stampOf(r.at);
  }

  // ====================================================== template registry
  // THE SERVER'S OWN JOB, FAKED.  A tag's capture template is the first `*'
  // heading of its config layer to EOF (`docs/capture.md', "Templates"); the
  // page never expands one and never holds template logic
  // (`docs/proposals/proposed/2026-08-24-the-capture-doc-is-the-material-doc.md',
  // "Refused").  So this table is what a GET beside `/properties' would answer:
  // the draft, already expanded, with `%^{…}' gone to empty slots, the stamping
  // escapes already stamped, and a note of where `%?' stood.
  var TEMPLATES = [
    {
      key: "i", tag: "", cycle: [],
      file: ".org-glance/config/system.org",
      where: "<root>/inbox.org",
      // THE DEFAULT TEMPLATE IS `* ' AND NOTHING ELSE, so the default DRAFT is
      // one row.  This is the number the whole spike turns on.
      skeleton: "* ",
      plan: false, props: [], body: false, point: "title",
      blurb: "the quick jot — no tag, no template, one line",
    },
    {
      key: "b", tag: "book", cycle: ["TODO", "READING", "READ"],
      file: ".org-glance/config/tags/book.org",
      where: "data/<shard>/…/data.org",
      skeleton: "#+TODO: TODO READING | READ\n* \n:PROPERTIES:\n"
              + ":AUTHOR: %^{Author}\n:END:\n%?",
      plan: false, props: ["AUTHOR"], body: true, point: "body",
      blurb: "a reading cycle, an author to fill, a body to write in",
    },
    {
      key: "m", tag: "meeting", cycle: ["TODO", "NEXT", "DONE"],
      file: ".org-glance/config/tags/meeting.org",
      where: "data/<shard>/…/data.org",
      skeleton: "#+TODO: TODO NEXT | DONE\n* \nSCHEDULED: %t\n:PROPERTIES:\n"
              + ":ATTENDEES: %^{Attendees}\n:END:\n%?",
      // `%t' IS STAMPED SERVER-SIDE, so the draft arrives with a real slot on
      // the planning line, so the date widget opens over a value to adjust.
      plan: true, props: ["ATTENDEES"], body: true, point: "body",
      blurb: "a drafted SCHEDULED slot and the room to name who is coming",
    },
  ];
  function templateFor(tag) {
    var t = String(tag || "").trim().toLowerCase();
    var hit = TEMPLATES.filter(function (x) { return x.tag === t; })[0];
    if (hit) return hit;
    // A TAG THAT DOES NOT EXIST YET IS STILL A TAG (`docs/capture.md:14'): the
    // server's charset wall refuses garbage, and an unknown tag falls back to
    // the default template — the same `* ' the inbox gets, worn on a headline.
    return {
      key: "?", tag: t, cycle: [], file: ".org-glance/config/system.org",
      where: "data/<shard>/…/data.org", skeleton: "* ",
      plan: false, props: [], body: false, point: "title",
      blurb: "a tag with no layer yet — the default template",
    };
  }
  var TAG_VOCAB = ["book", "meeting", "spike", "paper", "recipe", "person"];

  /** THE DRAFT: the bytes the server would serve for TAG, as a headline shape
   * with no file behind it.  It is the same shape `/headline' serves — which is
   * the proposal's whole point, and the reason the pane needs no second
   * renderer. */
  function makeDraft(tpl) {
    return {
      tpl: tpl, tag: tpl.tag,
      cycle: tpl.cycle.slice(),
      state: tpl.cycle.length ? tpl.cycle[0] : "",
      title: "",
      tags: tpl.tag ? [tpl.tag] : [],
      plan: tpl.plan ? [["SCHEDULED", stampOf(addDays(state.today, 1))]] : [],
      props: tpl.props.map(function (p) { return [p, ""]; }),
      body: tpl.body ? "" : null,
      point: tpl.point,
      // A ONE-ROW DRAFT: no planning, no drawer, no body.  Named because a law
      // below reads it (`bare').
      bare: !tpl.plan && !tpl.props.length && !tpl.body,
    };
  }

  // ================================================================== state
  var LOOK_DEFAULT = {
    first: "tag", then: "doc", place: "over",
    afterLand: null, grow: false,
  };
  var state = {
    look: null, today: null, phase: "idle",
    at: 0,                                 // the table's own walk
    draft: null, docRows: [], docAt: 0,
    edit: null, tag: null, cardAt: 0,
    jotTpl: null, editing: null,           // C: the row the doc is enriching
    mdoc: null, cue: null,
  };

  function el(id) { return document.getElementById(id); }
  function part(host, tag, cls, text) {
    var e = document.createElement(tag);
    if (cls) e.className = cls;
    if (text !== undefined) e.textContent = text;
    host.appendChild(e);
    return e;
  }
  function say(what) { var t = el("truth"); if (t) t.textContent = what; }

  // ============================================================= the table
  // SIX ROWS, the page's own strip, so every variant is argued against the same
  // background: the rows a capture is about to join.
  var ROWS = [
    { state: "TODO", title: "Read a date where a date is owed", tag: "spike",
      when: "<2026-08-26 Wed>" },
    { state: "READING", title: "The Iliad", tag: "book", when: "" },
    { state: "DONE", title: "Ship the fold marks", tag: "spike", when: "" },
    { state: "TODO", title: "Standup", tag: "meeting", when: "<2026-08-25 Tue>" },
    { state: "NEXT", title: "Rewrite the walk", tag: "spike", when: "" },
    { state: "READ", title: "Gödel, Escher, Bach", tag: "book", when: "" },
  ];
  var DONEISH = { DONE: 1, READ: 1 };
  function stateInk(s) { return DONEISH[s] ? "var(--g-done)" : "var(--g-todo)"; }

  function drawTable() {
    var host = el("tablewrap");
    if (!host) return;
    host.textContent = "";
    var t = part(host, "table", "tv-table", "");
    var thead = part(t, "thead", "", "");
    var hr = part(thead, "tr", "", "");
    part(hr, "th", "", "State");
    part(hr, "th", "tv-title", "Title");
    part(hr, "th", "", "Tag");
    part(hr, "th", "", "Scheduled");
    var body = part(t, "tbody", "", "");
    ROWS.forEach(function (r, i) {
      var tr = part(body, "tr", "", "");
      var cls = i % 2 ? "tv-alt" : "";
      if (i === state.at && state.phase === "idle") cls += " tv-sel";
      if (r.draft) cls += " tv-draft tv-sel";
      if (r.fresh) cls += " tv-fresh";
      tr.className = cls.trim();
      var td0 = part(tr, "td", "", "");
      if (r.state) {
        var pill = part(td0, "span", "tv-pill", r.state);
        pill.style.setProperty("--tv-badge", stateInk(r.state));
      }
      var td1 = part(tr, "td", "tv-title", "");
      td1.appendChild(document.createTextNode(
        r.title || (r.draft ? "" : "")));
      if (r.draft && !r.title)
        part(td1, "span", "tv-draftmark", "… the draft, not written yet");
      // THE CUE IS TRANSIENT, THE DOOR IS NOT (C): the word beside a fresh row
      // fades, and the key that opens the doc is the table's own either way.
      if (r.fresh && state.cue)
        part(td1, "span", "tv-cue" + (state.cue === "fade" ? " fade" : ""),
             state.look.afterLand);
      part(tr, "td", "tv-tag", r.tag ? ":" + r.tag + ":" : "");
      part(tr, "td", "", r.when);
      // B: THE DOC GROWS WHERE THE ROW WILL LIVE — a second `tr' under the
      // draft's own, one cell wide, in the table's own flow.
      if (r.draft && state.look.place === "inline" && state.mdoc) {
        var host2 = part(part(body, "tr", "", ""), "td", "cx-inline", "");
        host2.colSpan = 4;
        host2.appendChild(state.mdoc);
      }
    });
  }

  // ========================================================== the capture doc
  function docRow(cls, kind, build) {
    var d = document.createElement("div");
    d.className = "de " + cls;
    build(d);
    state.mdoc.appendChild(d);
    if (kind) state.docRows.push({ el: d, kind: kind });
    return d;
  }

  /** A SEED IS NOT A VALUE.  Where the expanded template left a slot empty the
   * pane draws the PLACE and says what it is for, in the ghost's own mute ink —
   * and marks with the caret's accent the one slot `%?' stood on. */
  function seed(host, text, isPoint) {
    part(host, "span",
         "cx-seed" + (isPoint ? " point cx-here" : ""), text);
  }

  function drawDoc() {
    var d = state.draft;
    state.mdoc.textContent = "";
    state.docRows = [];
    if (!d) return;

    docRow("d-head", "title", function (r) {
      part(r, "span", "ds", "* ");
      if (d.state) {
        var s = part(r, "span", "dc dc-state" + (DONEISH[d.state] ? " done" : ""),
                     d.state);
        s.id = "cxstate";
      }
      var t = part(r, "span", "dc dc-title", "");
      t.id = "cxtitle";
      if (d.title) t.textContent = d.title;
      else seed(t, "the headline", d.point === "title");
      var g = part(r, "span", "dc dc-tags", "");
      g.id = "cxtags";
      g.textContent = d.tags.length ? ":" + d.tags.join(":") + ":" : "";
    });

    // NO ENTRIES, NO LINE: the absence is the display.  A summon over a draft
    // that has none ghosts the keyword in, which is the shipped `DraftPlan'
    // machinery the proposal names under "Interactions".
    var ghosting = state.edit && state.edit.kind === "plan" && !d.plan.length;
    if (d.plan.length || ghosting) {
      var entries = d.plan.length ? d.plan : [["SCHEDULED", ""]];
      docRow("d-meta d-plan", "plan", function (r) {
        r.style.setProperty("--g-doc-indent", "2");
        entries.forEach(function (p, i) {
          if (i) r.appendChild(document.createTextNode(" "));
          var k = part(r, "span", "dk", "");
          k.appendChild(document.createTextNode(p[0]));
          part(k, "span", "dpunc", ":");
          r.appendChild(document.createTextNode(" "));
          var v = part(r, "span", "dv", p[1]);
          if (i === 0) v.id = "cxplan";
        });
      });
    }

    if (d.props.length) {
      var frame = docRow("d-meta d-drawer", null, function (r) {
        r.style.setProperty("--g-doc-indent", "2");
        var g = part(r, "span", "dg", "");
        part(g, "span", "dpunc dlead", ":");
        g.appendChild(document.createTextNode("PROPERTIES"));
        part(g, "span", "dpunc", ":");
      });
      d.props.forEach(function (p, i) {
        var pr = document.createElement("div");
        pr.className = "de d-meta";
        pr.style.setProperty("--g-doc-indent", "2");
        var k = part(pr, "span", "dk", "");
        part(k, "span", "dpunc dlead", ":");
        k.appendChild(document.createTextNode(p[0]));
        part(k, "span", "dpunc", ":");
        pr.appendChild(document.createTextNode(" "));
        // THE PAIR BOX COVERS THE WHOLE ROW, key and value together, the way
        // `#dpair' does (`Style.hs:328', the `left'/`right' inset) — so the
        // editor's host is the ROW and never the value's slot.  A box that
        // mounted in the value alone would draw the key twice: once as the
        // row's own text and once as the box's own field.
        pr.id = "cxpairrow" + i;
        var v = part(pr, "span", "dv", "");
        // THE EMPTY PROMPT PAIR — what `%^{Author}' became.  The pair is
        // DRAWN, with its value half empty, and the pair box edits it.
        if (p[1]) v.textContent = p[1];
        else seed(v, "…", false);
        frame.appendChild(pr);
        state.docRows.push({ el: pr, kind: "pair", i: i });
      });
      var end = document.createElement("div");
      end.className = "de d-meta";
      end.style.setProperty("--g-doc-indent", "2");
      var g2 = part(end, "span", "dg", "");
      part(g2, "span", "dpunc dlead", ":");
      g2.appendChild(document.createTextNode("END"));
      part(g2, "span", "dpunc", ":");
      frame.appendChild(end);
    }

    if (d.body !== null) {
      docRow("d-para", "body", function (r) {
        r.style.setProperty("--g-doc-indent", "2");
        r.id = "cxbody";
        if (d.body) r.textContent = d.body;
        else seed(r, "what you are capturing", d.point === "body");
      });
    }

    if (state.docAt >= state.docRows.length) state.docAt = state.docRows.length - 1;
    if (state.docAt < 0) state.docAt = 0;
    if (state.docRows.length) state.docRows[state.docAt].el.classList.add("dat");
    if (state.edit) mountEdit();
    repaint();
  }

  // ================================================================ editors
  // ONE EDITOR.  Every field below is the pane's own field wearing the pane's
  // own dress (`pane.css', transcribed from `Web/Page/Style.hs'), because the
  // capture doc IS the material doc: the same `RET' over the same rows, the
  // same pair box, the same date ghost.  Nothing here is capture-shaped.

  /** RET on the row at point opens the row's own editor. */
  function openEdit(kind) {
    if (state.edit) return;
    var row = state.docRows[state.docAt];
    if (!row) return;
    state.edit = { kind: kind || row.kind, i: row.i, virgin: true,
                   offers: [], offerAt: -1 };
    drawDoc();
  }

  function editHost() {
    var e = state.edit;
    if (!e) return null;
    if (e.kind === "title" || e.kind === "state") return el("cxtitle");
    if (e.kind === "tags") return el("cxtags");
    if (e.kind === "plan") return el("cxplan");
    if (e.kind === "pair") return el("cxpairrow" + e.i);
    if (e.kind === "body") return el("cxbody");
    return null;
  }

  function mountEdit() {
    var e = state.edit, d = state.draft, host = editHost();
    if (!host) { state.edit = null; return; }
    var rowEl = host.closest(".de");
    if (rowEl) rowEl.classList.add("cx-editing");

    // THE STATE DOOR STANDS WHERE THE STATE DOES, and ghosts a slot in where
    // the draft has no keyword yet — the same move the planning line makes.
    if (e.kind === "state") {
      var anchor = el("cxstate");
      if (anchor) anchor.textContent = "";
      else {
        anchor = document.createElement("span");
        anchor.className = "dc dc-state";
        anchor.id = "cxstate";
        host.parentNode.insertBefore(anchor, host);
      }
      mountStateDoor(anchor, rowEl);
      return;
    }

    host.textContent = "";
    // THE PLAN EDIT IS THE ONLY ONE THAT STANDS IN A SLOT: it is the date
    // widget, and the date widget mounts in the value's own place
    // (the date spike's round 2).  The rest are the pane's own boxes and cover
    // the row they write, edge to edge (`Style.hs', `#dpara,#dpair{left:…}').
    var wide = e.kind !== "plan";
    var slot = part(host, "span",
                    "cx-slot" + (wide ? " wide" : "")
                    + (e.kind === "pair" ? " dpair" : ""), "");

    if (e.kind === "body") {
      var ta = document.createElement("textarea");
      ta.className = "cx-field";
      ta.id = "cxfield";
      ta.rows = 3;
      ta.spellcheck = false;
      ta.value = d.body || "";
      ta.placeholder = "what you are capturing";
      slot.appendChild(ta);
      ta.focus();
      if (e.virgin) ta.setSelectionRange(0, ta.value.length);
      e.f = ta;
      return;
    }

    if (e.kind === "pair") {
      // THE PAIR BOX, `#dpair': the drawer's own two colons around a key field
      // that is EXACTLY AS WIDE AS WHAT IT HOLDS — monospace does the
      // arithmetic (`Style.hs', `#dpair #dkey'; `20-sheet.js' `pairMoved') —
      // and the value taking the rest.
      part(slot, "span", "dpunc dlead", ":");
      var kf = document.createElement("input");
      kf.className = "cx-field cx-key";
      kf.spellcheck = false;
      kf.value = d.props[e.i][0];
      var fitKey = function () {
        kf.style.flex = "none";
        kf.style.width = Math.max(1, kf.value.length) + "ch";
      };
      fitKey();
      kf.addEventListener("input", fitKey);
      slot.appendChild(kf);
      part(slot, "span", "dpunc", ":");
      var vf = document.createElement("input");
      vf.className = "cx-field cx-val";
      vf.id = "cxfield";
      vf.spellcheck = false;
      vf.value = d.props[e.i][1];
      vf.placeholder = "the value the prompt asked for";
      slot.appendChild(vf);
      e.f = vf; e.kf = kf;
      vf.focus();
      if (e.virgin) vf.setSelectionRange(0, vf.value.length);
      return;
    }

    var f = document.createElement("input");
    f.className = "cx-field";
    f.id = "cxfield";
    f.spellcheck = false;
    f.value = e.kind === "title" ? d.title
            : e.kind === "tags" ? d.tags.join(" ")
            : (d.plan[0] ? d.plan[0][1] : "");
    f.placeholder = e.kind === "title" ? "the headline's own text"
                  : e.kind === "tags" ? "tags, space separated"
                  : "today · +1w · fri · 2026-09-01";
    slot.appendChild(f);
    e.f = f;

    if (e.kind === "plan") {
      // THE DATE GHOST, `Style.hs:375' `.dgh': the resolution riding after what was
      // typed, mute, unselectable, and never the field's own value.  This is
      // the date-widget spike's picked look, reused whole — the capture doc
      // inherits it because it is the material doc.
      //
      // A GHOSTED FIELD IS EXACTLY AS WIDE AS WHAT IT HOLDS, so the resolution
      // lands one space after the last character typed rather than at some
      // column the layout picked.
      var ghost = part(slot, "span", "cx-ghost", "");
      ghost.id = "cxghost";
      ghost.setAttribute("aria-hidden", "true");
      e.ghost = ghost;
      var sizeAndGhost = function () {
        f.style.flex = "none";
        f.style.width = Math.min(40, Math.max(1, f.value.length) + 1) + "ch";
        var r = readDate(f.value);
        ghost.className = "cx-ghost" + (r.bad ? " bad" : "");
        // THREE STATES AND NO FOURTH: empty says nothing, a term still being
        // WRITTEN says nothing, a term that resolves shows the stamp, a term
        // the grammar refuses shows the refusal.  And NOTHING TO ADD where the
        // resolution IS what was typed — org's own spelling, kept.
        ghost.textContent = r.bad ? " ✗ " + r.bad
                          : r.ok && dateStamp(r) !== f.value.trim()
                            ? " → " + dateStamp(r) : "";
        repaint();
      };
      f.addEventListener("input", sizeAndGhost);
      sizeAndGhost();
    }
    if (e.kind === "title" || e.kind === "tags")
      f.addEventListener("input", function () { repaint(); });

    f.focus();
    // THE OPENING VALUE COMES UP WHOLLY SELECTED (the date-widget spike's
    // round 3), so the first character typed replaces the whole of it and `RET'
    // on an untouched field takes the default.
    if (e.virgin) f.setSelectionRange(0, f.value.length);
  }

  /** THE STATE DOOR, `t' (`Keymap.hs:103', `org-glance-overview:todo').  It
   * offers THE TAG'S OWN CYCLE, which rides in the same config file the
   * template does — which is why the destination question comes first. */
  function mountStateDoor(host, rowEl) {
    var e = state.edit, d = state.draft;
    if (!e.words) {
      // THE TAG'S OWN CYCLE, and the empty word that clears the state — a
      // draft with no `#+TODO:' layer behind it offers only that.
      e.words = d.cycle.slice().concat([""]);
      e.offerAt = Math.max(0, e.words.indexOf(d.state));
    }
    var slot = part(host, "span", "cx-slot", "");
    part(slot, "span", "cx-seed", d.state || "(no state)");
    var box = part(slot, "div", "doffer on", "");
    e.words.forEach(function (w, i) {
      var r = part(box, "div", i === e.offerAt ? "dof dat" : "dof", "");
      part(r, "span", "dow", w || "(no state)");
      part(r, "span", "dot", w ? (DONEISH[w] ? "done" : "active") : "clears it");
    });
    if (rowEl) rowEl.classList.add("cx-editing");
  }

  /** ESC CANCELS THE INPUT, AND THAT IS THE WHOLE OF WHAT IT DOES.  One press
   * abandons the open editor WHOLE — the menu is never a rung of its own — and
   * the row comes back spelled the way the edit found it.  With no editor open
   * the input IS THE CAPTURE, and one press drops it: no file ever existed, so
   * there is nothing to put back. */
  function closeEdit(take) {
    var e = state.edit, d = state.draft;
    if (!e) return true;
    if (take) {
      if (e.kind === "title") d.title = e.f.value.trim();
      else if (e.kind === "tags")
        d.tags = e.f.value.split(/[\s:]+/).filter(function (x) { return x; });
      else if (e.kind === "body") d.body = e.f.value;
      else if (e.kind === "pair") {
        d.props[e.i][0] = e.kf.value.trim().toUpperCase();
        d.props[e.i][1] = e.f.value.trim();
      } else if (e.kind === "state") {
        d.state = e.words[e.offerAt];
      } else if (e.kind === "plan") {
        var r = readDate(e.f.value);
        // REFUSALS BELONG ABOVE THE COMMIT: the box is not shut, so what was
        // typed is still on screen to be fixed (`20-sheet.js' `pairRefused').
        if (r.bad) { say(r.bad); return false; }
        // EMPTY CLEARS THE ENTRY, the shipped foot's own promise kept verbatim
        // (`Keymap.hs:138'; `30-capture.js:170').
        if (r.clear) d.plan = [];
        else d.plan = [["SCHEDULED", dateStamp(r)]];
      }
    }
    state.edit = null;
    // POINT IS SPENT ONCE: the `%?' mark is where the doc OPENED, not a place
    // the reader keeps returning to.
    if (take) d.point = "";
    drawDoc();
    if (state.mdoc) {
      var stale = state.mdoc.querySelectorAll(".cx-editing");
      for (var i = 0; i < stale.length; i += 1)
        stale[i].classList.remove("cx-editing");
    }
    say(take ? "" : "the edit was dropped — the row is what it was");
    return true;
  }

  // ================================================ commit, cancel, receipt
  /** THE ORG THAT WOULD LAND.  The page's own cargo (title, planning,
   * properties, body, tag) and, in mute, the server's own minting: the id, the
   * creation stamp, the shard path (`docs/capture.md', "What a tagged capture
   * writes").  The page spells none of those, which is why they are drawn as
   * someone else's ink. */
  function receipt(d) {
    var now = new Date();
    var host = el("landed");
    if (!host) return;
    host.textContent = "";
    host.className = "on";
    var tpl = d.tpl;
    part(host, "div", "path", tpl.tag ? "data/7f/3a91c2…/data.org"
                                      : "<root>/inbox.org");
    var head = part(host, "div", "", "");
    part(head, "span", "lead", "* ");
    if (d.state) part(head, "span", "kw", d.state + " ");
    head.appendChild(document.createTextNode(d.title));
    if (d.tags.length)
      part(head, "span", "tag", "    :" + d.tags.join(":") + ":");
    d.plan.forEach(function (p) {
      var l = part(host, "div", "", "");
      part(l, "span", "tok", p[0] + ": ");
      l.appendChild(document.createTextNode(p[1]));
    });
    part(host, "div", "tok", ":PROPERTIES:");
    part(host, "div", "srv", ":ORG_GLANCE_ID: 7f3a91c2-5d0e-4a1b-9f77-2c6e1b0a44d3");
    part(host, "div", "srv", ":ORG_GLANCE_CREATION_TIME: "
         + inactive(state.today, now.getHours(), now.getMinutes()));
    d.props.forEach(function (p) {
      if (!p[1]) return;
      var l = part(host, "div", "", "");
      part(l, "span", "tok", ":" + p[0] + ": ");
      l.appendChild(document.createTextNode(p[1]));
    });
    part(host, "div", "tok", ":END:");
    if (d.body) part(host, "div", "", d.body);
    part(host, "div", "srv",
         "\nmeta/EXTERNAL.jsonl  ← one line appended: the contract by which "
         + "Emacs learns of writes it did not make");
  }

  /** `C-c C-c' commits the draft WHOLE, through today's `capture' command —
   * the one door that mints the blob, the shard path, the creation drawer and
   * the ledger note (`Keymap.hs:129'; `docs/capture.md'). */
  function commitCapture() {
    if (state.phase !== "doc") { say("nothing is being captured"); return; }
    // AN OPEN EDIT COMMITS WITH THE DRAFT: `C-c C-c' over a field takes what is
    // in it first, so the reader never has to close an edit to commit.  A
    // REFUSED field stops the commit and keeps itself open.
    if (state.edit && !closeEdit(true)) return;
    var d = state.draft;
    if (!d) return;
    // A HEADLINE WITH NO TITLE IS NO HEADLINE, and the refusal is the shipped
    // one, word for word (`30-capture.js:120').
    if (!d.title) {
      say("nothing to capture — the headline has no title");
      // Put point where the fix is.
      state.docAt = 0; drawDoc();
      return;
    }
    if (state.editing) {
      // C: the doc opened over a row that already landed, so this is an EDIT.
      var row = state.editing;
      row.state = d.state; row.title = d.title;
      row.when = d.plan.length ? d.plan[0][1] : "";
      state.editing = null;
      shutDoc();
      drawTable();
      receipt(d);
      say("row updated · " + d.title);
      return;
    }
    receipt(d);
    shutDoc();
    land(d, false);
    say("captured · " + (d.tag ? ":" + d.tag + ":" : d.tpl.where));
  }

  /** THE ROW THE CAPTURE MINTED, delivered the way the watch delivers one
   * (`docs/capture.md', "What a tagged capture writes").  ONE ROW IS FRESH AT A
   * TIME: the cue is about the capture that just happened. */
  function land(d, cue) {
    ROWS.forEach(function (r) { delete r.fresh; });
    ROWS.unshift({ state: d.state, title: d.title, tag: d.tag,
                   when: d.plan.length ? d.plan[0][1] : "", fresh: true });
    state.at = 0;
    state.cue = cue ? "on" : null;
    if (cue) setTimeout(function () {
      if (state.cue === "on") { state.cue = "fade"; drawTable(); }
    }, 6000);
    drawTable();
  }

  /** ESC LEAVES NOTHING.  No file ever existed, so the born-at-open memory is
   * trivially empty: there is no draft store, no autosave, and no half-written
   * blob to reconcile (the proposal's "Refused"). */
  function cancelAll(word) {
    state.edit = null;
    shutDoc();
    drawTable();
    say(word || "nothing was written — no file ever existed");
  }

  function shutDoc() {
    state.phase = "idle";
    state.draft = null; state.edit = null; state.docRows = []; state.docAt = 0;
    state.tag = null; state.editing = null; state.jotTpl = null;
    // B'S DRAFT ROW IS THE DRAFT: when the draft goes the row goes with it, so
    // ESC leaves the strip exactly the six rows it found.
    for (var i = ROWS.length - 1; i >= 0; i -= 1)
      if (ROWS[i].draft) ROWS.splice(i, 1);
    if (state.mdoc && state.mdoc.parentNode)
      state.mdoc.parentNode.removeChild(state.mdoc);
    state.mdoc = null;
    unraise();
    repaint();
  }

  // ============================================================== the doors
  function raise(cls, head, foot, build) {
    var p = el("prompt");
    p.className = "on";
    el("pbox").className = cls || "";
    el("phead").textContent = "";
    el("phead").appendChild(document.createTextNode(head));
    el("pfoot").textContent = foot;
    var b = el("pbody");
    b.textContent = "";
    build(b);
    return b;
  }
  function unraise() {
    var p = el("prompt");
    if (p) { p.className = ""; el("pbody").textContent = ""; }
  }
  function badge(text) {
    var b = document.createElement("span");
    b.className = "cx-badge";
    b.textContent = text;
    el("phead").appendChild(b);
  }

  /** PART 1 OF THE SHIPPED FORM, AND THE ONLY PART EVERY VARIANT KEEPS: the tag
   * field, completing over the tree's own vocabulary, an EMPTY TAG THE INBOX
   * (`docs/capture.md:14'…`:18'; `30-capture.js:49' `drawTagList').  The
   * destination question precedes the doc because the tag picks the template,
   * the `#+TODO:' cycle, and where the blob lands. */
  function openTag() {
    state.phase = "tag";
    state.tag = { hot: -1, shown: [] };
    // The foot is the shipped one, word for word (`30-capture.js:26').
    raise("", "capture · tag",
          "RET moves on · an empty tag is the inbox · ESC leaves",
          function (b) {
      var f = document.createElement("input");
      f.id = "ktag";
      f.spellcheck = false;
      f.placeholder = "tag — empty is the inbox";
      b.appendChild(f);
      part(b, "div", "", "").id = "klist";
      f.addEventListener("input", function () {
        state.tag.hot = -1; drawTagList();
      });
      f.focus();
    });
    drawTagList();
    repaint();
  }
  function drawTagList() {
    var box = el("klist"), f = el("ktag");
    if (!box || !f) return;
    var want = f.value.trim().toLowerCase();
    state.tag.shown = TAG_VOCAB.filter(function (t) {
      return !want || t.indexOf(want) !== -1;
    }).slice(0, 8);
    if (state.tag.hot >= state.tag.shown.length) state.tag.hot = -1;
    box.textContent = "";
    state.tag.shown.forEach(function (t, i) {
      var r = part(box, "div", i === state.tag.hot ? "ke kh" : "ke", t);
      var tpl = templateFor(t);
      r.textContent = t + (tpl.cycle.length ? "   " + tpl.cycle.join(" ") : "");
    });
  }
  /** RET IS DRY THEN FINAL where a menu stands: the first press takes the
   * highlighted offer INTO the field and does nothing else; the second settles
   * the tag (`20-sheet.js' `takeOffer'; the dot-chain spike's rounds 3/11/15). */
  function tagRet() {
    var f = el("ktag");
    if (state.tag.hot >= 0) {
      var w = state.tag.shown[state.tag.hot];
      if (w !== f.value.trim()) {
        f.value = w; state.tag.hot = -1; drawTagList();
        say("taken · RET again settles it");
        return;
      }
    }
    settleTag(f.value);
  }
  function settleTag(text) {
    var tpl = templateFor(text);
    if (state.look.then === "jot") openJot(tpl);
    else openDoc(tpl);
  }

  /** PART 3 OF THE SHIPPED FORM: one line, `RET' captures (`30-capture.js:105').
   * C keeps it as the fast path; D keeps it as the doc's LARVAL STAGE. */
  function openJot(tpl) {
    state.phase = "jot";
    state.jotTpl = tpl;
    var foot = state.look.grow
      ? "RET captures · a structure key grows this into the doc · ESC leaves"
      : "RET captures · ESC leaves";
    raise("", "capture · " + (tpl.tag ? ":" + tpl.tag + ":" : "inbox"),
          foot, function (b) {
      var f = document.createElement("textarea");
      f.id = "ktext";
      f.rows = 2;
      f.spellcheck = false;
      f.placeholder = "the line — it lands where %? stood";
      b.appendChild(f);
      if (state.look.grow) {
        var g = part(b, "div", "", "");
        g.id = "kgrow";
        g.innerHTML = "structure: <b>C-c C-s</b> a date · <b>:</b> at the "
                    + "line's start, a property · <b>M-RET</b> the whole doc";
      }
      f.focus();
      if (state.look.grow) f.addEventListener("input", growWatch);
    });
    repaint();
  }

  /** D'S OWN DOOR.  A `:' TYPED AT THE LINE'S START is org's own way of
   * beginning a drawer line, so it means the same here: the reader has asked
   * for structure, and the form hatches. */
  function growWatch() {
    var f = el("ktext");
    if (!f) return;
    if (/^:/.test(f.value)) {
      grow(f.value.replace(/^:+/, "").trim(), "pair",
           "a “:” at the line's start asked for a property");
    }
  }
  function grow(text, into, why) {
    var tpl = state.jotTpl || templateFor("");
    openDoc(tpl);
    // THE LARVAL FORM'S FIELD IS NOT THE DOC'S.  It is dropped before the doc
    // chooses its own point, or an empty title field would write itself over
    // the line the moult just carried.
    state.edit = null;
    // THE FORM IS THE DOC'S LARVAL STAGE: what was typed IS the title, carried
    // whole across the moult.  A line thrown away here is a line the reader
    // types twice.
    state.draft.title = String(text || "").trim();
    state.draft.point = "";
    state.docAt = 0;
    drawDoc();
    if (into === "pair") {
      if (!state.draft.props.length) state.draft.props = [["", ""]];
      drawDoc();
      state.docAt = firstOf("pair");
      drawDoc();
      openEdit("pair");
    } else if (into === "plan") {
      summonPlan();
    }
    say(why || "grown into the doc");
  }
  function firstOf(kind) {
    for (var i = 0; i < state.docRows.length; i += 1)
      if (state.docRows[i].kind === kind) return i;
    return 0;
  }

  /** E'S OWN DOOR: the templates as cards — the tag, the cycle, the skeleton it
   * expands from, and where the blob lands.  For a reader with many tags this
   * is the one screen that says what picking one MEANS; for the inbox jot it is
   * a step, which is E's whole cost and is why `RET' on nothing goes straight
   * to the inbox. */
  function openGallery() {
    state.phase = "gallery";
    state.cardAt = 0;
    // POINT OPENS ON THE INBOX CARD, which is what keeps the quick jot at `+'
    // and `RET' — the same two keys it costs today — while every other template
    // is one letter away.
    raise("gallery", "capture · template",
          "n p walk · i b m pick by letter · RET takes the card at point — "
          + "point starts on the inbox, so a jot is still + RET · "
          + "/ a tag not shown · ESC leaves",
          function (b) {
      var g = part(b, "div", "", "");
      g.id = "cards";
    });
    drawCards();
    repaint();
  }
  function drawCards() {
    var g = el("cards");
    if (!g) return;
    g.textContent = "";
    TEMPLATES.forEach(function (t, i) {
      var c = part(g, "div", i === state.cardAt ? "cg at" : "cg", "");
      var h = part(c, "div", "cg-head", "");
      part(h, "span", "cg-key", t.key);
      part(h, "span", "cg-tag", t.tag ? ":" + t.tag + ":" : "(empty) inbox");
      part(h, "span", "cg-cycle", t.cycle.length ? t.cycle.join(" ") : "no cycle");
      part(c, "pre", "cg-skel", t.skeleton);
      part(c, "div", "cg-where", "→ " + t.where + "   ·   " + t.blurb);
      // THE LAYER FILE'S OWN NAME, which is the file the settings sheet edits
      // (`docs/capture.md', "Templates").  A card is where it can be said; a
      // completing field has nowhere to put it.
      part(c, "div", "cg-where", t.file);
    });
  }

  /** THE SHEET.  The pane, over the page, holding the draft — the same pane
   * that draws any doc, because the draft IS a doc. */
  function openDoc(tpl) {
    state.phase = "doc";
    state.draft = makeDraft(tpl);
    state.docAt = 0;
    state.mdoc = document.createElement("div");
    state.mdoc.id = "mdoc";
    if (state.look.place === "inline") {
      // B: the draft is a ROW in the strip before it is a blob on disk, and the
      // doc grows beneath its own row.  THE POPUP LEAVES WITH THE TAG: B's
      // whole claim is that nothing is raised over the page, so the door the
      // tag was asked through shuts before the doc appears.
      unraise();
      ROWS.unshift({ state: state.draft.state, title: "", tag: tpl.tag,
                     when: state.draft.plan.length ? state.draft.plan[0][1] : "",
                     draft: true });
      drawTable();
    } else {
      raise("sheet", "capture · "
            + (tpl.tag ? ":" + tpl.tag + ":" : "inbox"),
            footWords(), function (b) {
        b.appendChild(state.mdoc);
      });
      badge("draft · no file yet");
    }
    // POINT LANDS WHERE `%?' STOOD (the proposal's flow, step 3), and it lands
    // WITH THE EDITOR OPEN — a capture whose first keystroke is `RET' would be
    // a form with extra steps.  The rows have to exist before one can be picked,
    // so the draw comes first and the pick reads it.
    state.docAt = 0;
    drawDoc();
    state.docAt = state.draft.point === "body" ? firstOf("body") : 0;
    drawDoc();
    openEdit(state.draft.point === "body" ? "body" : "title");
    repaint();
  }
  function footWords() {
    return "n p walk · RET edits the row · t state · : tags · C-c C-s a date · "
         + "C-c C-c commits · ESC leaves nothing";
  }

  /** `C-c C-s' over a draft: the shipped `DraftPlan' machinery, which is
   * exactly what capture's SCHEDULED slot is before commit (the proposal's
   * "Interactions").  With no planning line the keyword is ghosted in for the
   * length of the edit and leaves with it. */
  function summonPlan() {
    if (state.phase !== "doc") { say("no draft to plan"); return; }
    if (state.edit && !closeEdit(true)) return;
    state.edit = { kind: "plan", virgin: true, offers: [], offerAt: -1 };
    drawDoc();
    // POINT FOLLOWS THE SUMMON onto the line it ghosted in, so `ESC' hands the
    // keys back where the reader is looking.
    var i = firstOf("plan");
    if (state.docRows[i] && state.docRows[i].kind === "plan") {
      if (state.docRows[state.docAt])
        state.docRows[state.docAt].el.classList.remove("dat");
      state.docAt = i;
      state.docRows[i].el.classList.add("dat");
      repaint();
    }
  }

  // =================================================================== keys
  var NEXT = { n: 1, j: 1 }, PREV = { p: 1, k: 1 };
  var prefix = false;

  function inField() {
    var a = document.activeElement;
    return !!a && (a.tagName === "INPUT" || a.tagName === "TEXTAREA");
  }

  function keys(e) {
    var k = e.key;

    // `C-c C-c' commits, `C-c C-s' plans (`Keymap.hs:129', `:123').  The prefix
    // is checked FIRST so the chord's own second half is not eaten as a new
    // prefix.
    if (e.ctrlKey && (k === "c" || k === "C")) {
      if (prefix) { prefix = false; commitCapture(); }
      else { prefix = true; say("C-c —"); }
      e.preventDefault(); return;
    }
    if (prefix && e.ctrlKey && (k === "s" || k === "S")) {
      prefix = false; e.preventDefault();
      if (state.phase === "jot" && state.look.grow) {
        grow(el("ktext").value, "plan",
             "C-c C-s asked for a date, so the form became the doc");
        return;
      }
      summonPlan();
      return;
    }
    prefix = false;

    // ESC IS ONE LAW ON EVERY SURFACE: it cancels the input it is in, WHOLE.
    // A menu is never a rung of its own; the editor is the input while one is
    // open, and the CAPTURE is the input when none is.
    if (k === "Escape") {
      e.preventDefault();
      if (state.edit) {
        // THE BARE DRAFT'S OWN ESC, and the twin of its `RET' below: on a
        // one-row draft the title edit IS the capture, so there is no inner
        // input to escape from and the press drops the whole of it.  That keeps
        // today's jot exactly two keys wide either way — `+ RET text RET' to
        // capture, `+ RET ESC' to abandon.
        if (state.draft && state.draft.bare && !state.editing
            && state.edit.kind === "title") { cancelAll(); return; }
        closeEdit(false); return;
      }
      if (state.phase !== "idle") { cancelAll(); return; }
      say("nothing is open");
      return;
    }

    // M-RET, org's own `org-insert-heading': the key that ASKS FOR STRUCTURE.
    if (e.altKey && k === "Enter") {
      e.preventDefault();
      if (state.phase === "jot" && state.look.grow) {
        grow(el("ktext").value, "doc",
             "M-RET asked for the whole doc, and the line came with it");
        return;
      }
      say("M-RET is D's own door");
      return;
    }
    if (e.ctrlKey || e.altKey || e.metaKey) return;

    if (state.phase === "idle") return idleKeys(e, k);
    if (state.phase === "tag") return tagKeys(e, k);
    if (state.phase === "gallery") return galleryKeys(e, k);
    if (state.phase === "jot") return jotKeys(e, k);
    return docKeys(e, k);
  }

  function idleKeys(e, k) {
    if (k === "+") { openCapture(); e.preventDefault(); return; }
    if (NEXT[k] || k === "ArrowDown") walkTable(1);
    else if (PREV[k] || k === "ArrowUp") walkTable(-1);
    else if (state.look.afterLand && (k === "o" || k === "Enter")) {
      // C: THE DOOR IS THE TABLE'S OWN KEY.  `o' is asked for, and `RET' is
      // what the table already spends on this (`Keymap.hs:61',
      // `org-glance-overview:materialize') — both open the doc here, and the
      // README names the collision `o' walks into (`Keymap.hs:83').
      openOverFresh(k);
      e.preventDefault(); return;
    } else if (k === "~") { theme(); }
    else return;
    e.preventDefault();
  }
  function walkTable(d) {
    state.at = Math.max(0, Math.min(ROWS.length - 1, state.at + d));
    drawTable(); repaint();
  }
  function openCapture() {
    if (state.look.first === "gallery") openGallery();
    else openTag();
  }
  function openOverFresh(k) {
    var row = ROWS[state.at];
    if (!row || !row.fresh) {
      say(k === "o" ? "o opens this row's links (Keymap.hs:83) — "
                    + "there is no fresh capture here"
                    : "RET materializes the row at point");
      return;
    }
    var tpl = templateFor(row.tag);
    openDoc(tpl);
    // THE DOC OPENS OVER THE ROW THAT ALREADY LANDED, so it is seeded from the
    // row and not from the template's empty title.  This is a normal edit
    // afterwards: the capture was committed by the `RET' that landed the line.
    state.draft.title = row.title;
    state.draft.state = row.state;
    state.draft.point = "";
    state.editing = row;
    state.docAt = 0;
    // The doc opens on the WALK rather than in a field: there is nothing to
    // seed, and the reader came here to add structure, not to retype the title.
    state.edit = null;
    drawDoc();
    say("the doc, over a row that already landed — C-c C-c saves the edit");
  }

  function tagKeys(e, k) {
    if (k === "Enter") { tagRet(); e.preventDefault(); return; }
    if (k === "ArrowDown" || k === "Tab") {
      state.tag.hot = Math.min(state.tag.shown.length - 1, state.tag.hot + 1);
      drawTagList(); e.preventDefault(); return;
    }
    if (k === "ArrowUp") {
      state.tag.hot = Math.max(-1, state.tag.hot - 1);
      drawTagList(); e.preventDefault(); return;
    }
  }

  function galleryKeys(e, k) {
    if (k === "Enter") {
      settleTag(TEMPLATES[state.cardAt].tag);
      e.preventDefault(); return;
    }
    if (NEXT[k] || k === "ArrowDown" || k === "ArrowRight") {
      state.cardAt = Math.min(TEMPLATES.length - 1, state.cardAt + 1);
      drawCards(); e.preventDefault(); return;
    }
    if (PREV[k] || k === "ArrowUp" || k === "ArrowLeft") {
      state.cardAt = Math.max(0, state.cardAt - 1);
      drawCards(); e.preventDefault(); return;
    }
    var by = TEMPLATES.filter(function (t) { return t.key === k; })[0];
    if (by) { settleTag(by.tag); e.preventDefault(); return; }
    if (k === "/") {
      // `/' ALWAYS NARROWS (`Keymap.hs:62'): the one door into a field, here
      // for a tag no card shows.
      openTag(); e.preventDefault(); return;
    }
  }

  function jotKeys(e, k) {
    if (k === "Enter") {
      e.preventDefault();
      var f = el("ktext");
      var typed = f.value.trim();
      if (!typed) { say("nothing to capture"); return; }
      // C AND D: `RET' IS THE REAL COMMIT, exactly as it is today
      // (`30-capture.js:105'…`:130').  Nothing is staged, nothing is a draft:
      // the row lands.
      var tpl = state.jotTpl;
      var d = makeDraft(tpl);
      d.title = typed;
      receipt(d);
      shutDoc();
      land(d, !!state.look.afterLand);
      say("captured · " + (tpl.tag ? ":" + tpl.tag + ":" : tpl.where)
          + (state.look.afterLand ? " · " + state.look.afterLand : ""));
    }
  }

  function docKeys(e, k) {
    var d = state.draft;
    if (state.edit) {
      var ed = state.edit;
      if (ed.kind === "state") {
        if (NEXT[k] || k === "ArrowDown") {
          ed.offerAt = Math.min(ed.words.length - 1, ed.offerAt + 1);
          drawDoc(); e.preventDefault(); return;
        }
        if (PREV[k] || k === "ArrowUp") {
          ed.offerAt = Math.max(0, ed.offerAt - 1);
          drawDoc(); e.preventDefault(); return;
        }
        if (k === "Enter") { closeEdit(true); e.preventDefault(); return; }
        e.preventDefault(); return;
      }
      if (k === "Enter" && !(ed.kind === "body" && e.shiftKey)) {
        e.preventDefault();
        // THE BARE DRAFT'S OWN RET.  A draft with one row and one field has
        // nothing else to close into, so `RET' there commits the capture — the
        // quick jot's muscle memory kept without a second surface.  See the
        // README's finding 1; this is the amendment the spike proposes.
        if (ed.kind === "title" && d.bare && !state.editing) {
          closeEdit(true);
          commitCapture();
          return;
        }
        closeEdit(true);
        return;
      }
      return;                                   // every other key is a character
    }
    if (NEXT[k] || k === "ArrowDown") walkDoc(1);
    else if (PREV[k] || k === "ArrowUp") walkDoc(-1);
    else if (k === "Enter") openEdit(null);
    else if (k === "t") { state.docAt = 0; drawDoc(); openEdit("state"); }
    else if (k === ":") { state.docAt = 0; drawDoc(); openEdit("tags"); }
    else if (k === "~") theme();
    else return;
    e.preventDefault();
  }
  function walkDoc(d) {
    if (!state.docRows.length) return;
    state.docRows[state.docAt].el.classList.remove("dat");
    state.docAt = Math.max(0, Math.min(state.docRows.length - 1, state.docAt + d));
    state.docRows[state.docAt].el.classList.add("dat");
    repaint();
  }

  function theme() {
    document.documentElement.dataset.theme =
      document.documentElement.dataset.theme === "dark" ? "light" : "dark";
  }

  // ================================================================== paint
  function repaint() {
    var t = el("state");
    if (!t) return;
    var d = state.draft;
    var bits = [];
    if (state.phase === "idle") bits.push("table · row " + (state.at + 1));
    else bits.push(state.phase);
    if (d) {
      bits.push(d.tag ? ":" + d.tag + ":" : "inbox");
      bits.push(state.docRows.length + (state.docRows.length === 1
                                        ? " row" : " rows"));
      if (state.docRows[state.docAt])
        bits.push("at " + state.docRows[state.docAt].kind);
      if (state.edit) bits.push("editing " + state.edit.kind);
    }
    // THE COST WITH A NUMBER ON IT: how wide the doc actually is where it
    // stands, which is the whole of B's trade-off said in pixels.
    if (state.mdoc && state.mdoc.clientWidth)
      bits.push(state.mdoc.clientWidth + "px wide");
    t.textContent = bits.join(" · ");
  }

  // ================================================================== mount
  function mount(opts) {
    state.look = {};
    Object.keys(LOOK_DEFAULT).forEach(function (k) {
      state.look[k] = LOOK_DEFAULT[k];
    });
    Object.keys((opts && opts.look) || {}).forEach(function (k) {
      state.look[k] = opts.look[k];
    });
    var n = new Date();
    state.today = { y: n.getFullYear(), m: n.getMonth() + 1, d: n.getDate() };
    drawTable();
    document.addEventListener("keydown", keys, true);
    addEventListener("resize", repaint);
    repaint();
    say("press + to capture");
    return state;
  }

  return {
    mount: mount,
    state: function () {
      return { phase: state.phase, tag: state.draft ? state.draft.tag : "",
               rows: state.docRows.length,
               at: state.docRows[state.docAt]
                     ? state.docRows[state.docAt].kind : "",
               edit: state.edit ? state.edit.kind : "",
               title: state.draft ? state.draft.title : "",
               width: state.mdoc ? state.mdoc.clientWidth : 0 };
    },
    theme: theme, readDate: readDate,
  };
})();
