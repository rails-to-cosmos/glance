    // The body's PARAGRAPHS: runs of non-blank lines, each remembering the LINE
    // RANGE it came out of.  The range is what makes an edit a splice — a commit
    // puts the paragraph's own lines back where they were and leaves every other
    // byte of the body alone, blank lines and odd spacing included.  Line 0 is
    // the headline's own and is never one of them (`blocksIn').
    // ORG'S LIST OPENERS, as the corpus actually spells them: `- ' at 28571
    // lines, `1.'/`1)' at 2675, `+ ' at 42 and an INDENTED `* ' at 34.  All four
    // are honoured because all four cost one alternation.  A `* ' at COLUMN 1 is
    // a headline rather than an item — the lens has taken those out already, but
    // the guard is kept here so the predicate is true on its own terms.
    const LIST_AT = /^(\s*)([-+*]|\d+[.)])(\s+|$)/;
    function listOpener(line) {
      const m = LIST_AT.exec(String(line));
      return m && !(m[2] === "*" && !m[1]) ? m : null;
    }
    // A BLOCK IS ANY `#+begin_X'/`#+end_X' PAIR, by name.  Naming quote, src and
    // example outright would have missed this corpus's most common block by a
    // factor of three: `pin' 1022, `src' 338, `quote' 111, `notes' 42,
    // `example' 38.  A reader's own block kind is as much a block as org's.
    const BEGIN_AT = /^\s*#\+begin_(\S+)/i;
    const closerOf = (name) => new RegExp(
      "^\\s*#\\+end_" + name.replace(/[.*+?^${}()|[\]\\]/g, "\\$&")
        + "\\s*$", "i");
    // Where the block ENDS, or nothing: an listOpener with no closer under it is
    // ordinary text, since guessing an end would put a stop around bytes org
    // itself reads as a paragraph.
    function blockRun(lines, i, end) {
      const shut = closerOf(BEGIN_AT.exec(lines[i])[1]);
      for (let j = i + 1; j < end; j += 1) if (shut.test(lines[j])) return j + 1;
      return -1;
    }
    // AN ORG TABLE IS A RUN OF `|' LINES, the opening bar being the whole of what
    // says so: org's own rule (`org-table-any-line-regexp'), the same one that
    // makes a `|---+---|' RULE a row of the table rather than an end to it.  A
    // run of them is ONE COARSE STOP with its lines under it, which is the LIST'S
    // shape exactly — `[whole, row1..rowN]' — so the walk, the draw, the flags,
    // `o' and the splice all reach it through what is already there.
    // A LINE IS A LEAF, and that is the whole grain: 101 of this corpus's 6337
    // files hold table rows (2178 lines, 211 of them rules), so a table is real
    // and rare and a cell grain would be a second walk to teach for one file in
    // sixty.  Editing a row is editing its line, which is what org's own table
    // editor comes to once the alignment is left to org.
    const TABLE_AT = /^\s*\|/;
    // What stays INSIDE a list once it has opened: another item at any depth, or
    // an indented continuation line.  An unindented line that is not an item
    // ends it.
    const rides = (line) => !!listOpener(line) || /^\s/.test(String(line));
    // A LIST RUN and its TOP-LEVEL items.  The base indent is the FIRST item's,
    // and an item deeper than it rides inside the item above rather than taking a
    // stop of its own — v1's grain, and the nesting is still there in the text.
    //
    // ONE BLANK LINE STAYS IN, which is org's own rule and the corpus's: 1173
    // item pairs are separated by exactly one.  Two, or a blank with something
    // else under it, close the list.
    function listRun(lines, i, end) {
      const base = listOpener(lines[i])[1].length;
      const items = [];
      let at = i, from = -1, last = i;
      while (at < end) {
        if (String(lines[at]).trim() === "") {
          let j = at;
          while (j < end && String(lines[j]).trim() === "") j += 1;
          if (j - at > 1 || j >= end || !rides(lines[j])) break;
          at = j; continue;
        }
        const m = listOpener(lines[at]);
        if (m && m[1].length <= base) {
          if (from !== -1) items.push({ from, to: last });
          from = at;
        } else if (!rides(lines[at])) break;
        at += 1; last = at;
      }
      if (from !== -1) items.push({ from, to: last });
      return { to: last, items };
    }
    // Blank-separated runs of A..B, which is what a plain paragraph is and what
    // a block's interior is cut into.
    function runsIn(lines, a, b) {
      const out = [];
      let from = -1;
      for (let i = a; i <= b; i += 1) {
        const blank = i === b || String(lines[i]).trim() === "";
        if (!blank) { if (from === -1) from = i; continue; }
        if (from === -1) continue;
        out.push({ from, to: i, text: lines.slice(from, i).join("\n") });
        from = -1;
      }
      return out;
    }
    // THE WALK SEQUENCE, in document order, with the COMPOSITES INLINE: a list,
    // a block or a table is `[whole, part1..partN]', so `n' from above meets the
    // whole thing first and then walks into it, and `p' from below walks the
    // parts and meets the whole on the way out.  That falls out of one flat list
    // — there is no descend key and no ascend key, and `p' is `n' read backwards
    // because the sequence is the same sequence.
    //
    // A plain paragraph is one stop, as it always was.  Line 0 is the headline's
    // own, which the lens leaves at the head of the body: it is the headline
    // ELEMENT's, drawn from the cells the server sent, and it is never a
    // paragraph.  OWN is where this entry's own text stops and the outline under
    // it begins — the server's `ownLines', since the page holds no parser and the
    // same bytes must not be drawn twice, once as a paragraph and once as the
    // child that owns them.
    function blocksIn(lines, own) {
      const out = [];
      const end = Math.max(0, Math.min(own, lines.length));
      const cut = (a, b) => lines.slice(a, b).join("\n");
      // `up' is the leaf's IMMEDIATE parent as an index into OUT — the composite
      // for a top item, the item above for a nested one — which is what makes
      // the grain a LADDER rather than two rungs: `f' descends one step, `b'
      // ascends one, and the walk clamps to one parent's run.
      const whole = (a, b, name, leaves) => {
        const at = out.length;
        out.push({ from: a, to: b, text: cut(a, b), grain: "composite", name });
        for (const p of leaves)
          out.push({ from: p.from, to: p.to, text: p.text, grain: "leaf",
                     up: at });
      };
      // A list item's own DEEPER runs, as leaf nodes under it — the recursion
      // that makes `- alpha' with `  - nested' inside it one more rung down
      // rather than opaque text.  The item's range is untouched: the sub-leaves
      // are stops WITHIN it, and the draw and the splice both read the ladder.
      const pushItem = (from, to, up) => {
        const at = out.length;
        out.push({ from, to, text: cut(from, to), grain: "leaf", up });
        const base = listOpener(lines[from])[1].length;
        for (let n = from + 1; n < to; n += 1) {
          const m = listOpener(lines[n]);
          if (m && m[1].length > base) {
            const run = listRun(lines, n, to);
            for (const it of run.items) pushItem(it.from, it.to, at);
            n = run.to - 1;
          }
        }
      };
      let i = 1;
      while (i < end) {
        if (String(lines[i]).trim() === "") { i += 1; continue; }
        if (BEGIN_AT.test(lines[i])) {
          const shut = blockRun(lines, i, end);
          if (shut !== -1) {
            whole(i, shut, BEGIN_AT.exec(lines[i])[1].toLowerCase(),
                  runsIn(lines, i + 1, shut - 1));
            i = shut; continue;
          }
        }
        // Every line its own leaf, which is the one place a table differs from a
        // list: a list's items are RUNS and a table's rows are LINES, so the
        // leaves are cut here rather than by a scan of what rides inside them.
        if (TABLE_AT.test(lines[i])) {
          let j = i;
          while (j < end && TABLE_AT.test(lines[j])) j += 1;
          const rows = [];
          for (let n = i; n < j; n += 1)
            rows.push({ from: n, to: n + 1, text: cut(n, n + 1) });
          whole(i, j, "table", rows);
          i = j; continue;
        }
        if (listOpener(lines[i])) {
          const run = listRun(lines, i, end);
          const at = out.length;
          out.push({ from: i, to: run.to, text: cut(i, run.to),
                     grain: "composite", name: "list" });
          for (const it of run.items) pushItem(it.from, it.to, at);
          i = run.to; continue;
        }
        // A paragraph stops where the next STRUCTURE opens as readily as at a
        // blank line: org lets a list follow its lead-in with no blank between.
        let j = i + 1;
        while (j < end && String(lines[j]).trim() !== ""
               && !listOpener(lines[j]) && !BEGIN_AT.test(lines[j])
               && !TABLE_AT.test(lines[j])) j += 1;
        out.push({ from: i, to: j, text: cut(i, j), grain: "element" });
        i = j;
      }
      return out;
    }
    // The document H makes, in FILE ORDER: the headline line, the body's own
    // paragraphs, then the children hanging under it.  The cursor is kept BY ID
    // across a rebuild, so a commit that re-materializes lands the reader back on
    // the element they were working on rather than at the top.
    function docFrom(h) {
      // ORG-STARTUP-INDENTED'S OTHER HALF: content lines sit under the TITLE
      // TEXT rather than under the stars.  The head always draws `* ', being the
      // root of its own document whatever entry the sheet walked into, so the
      // column is the width of that prefix and is DERIVED from `dstars' rather
      // than spelled as a 2 beside it, which keeps the two rules from drifting.
      // Written as a NUMBER onto the pane, the way the log's cap is: the
      // arithmetic lives in the stylesheet, once.
      el("mdoc").style.setProperty("--g-doc-indent",
                                    String(dstars(docLevel()).length));
      const was = drows[dat] ? drows[dat].id : null;
      drows = [];
      dlines = String(h.body || "").split("\n");
      drows.push({ id: "H", kind: "head", cells: cellsOf(h.cells) });
      const own = h.ownLines === undefined ? dlines.length : h.ownLines;
      // AN ELEMENT ID IS ITS PLACE IN THIS BUILD, re-issued every time the
      // document is built: `B0' is the first paragraph of whatever the body holds
      // NOW.  The cursor, the flags and a stash restore are all keyed by it, so
      // an edit that changes how many elements sit ABOVE one moves that one's id
      // and a reader lands on whatever now carries the ordinal.  Stable enough
      // for what reads it — a build lasts from one commit to the next, and a
      // commit re-materializes and re-draws in one step — and a different rule
      // from the panel's, where `P<n>' is handed out once and nothing rebuilds
      // the drawer under the reader.  The counter is the loop's, that being the
      // whole of its life.
      // `owner' is the IMMEDIATE parent's row id, off the builder's `up' index:
      // the composite for a top item, the item above for a nested one.  The
      // sibling walk, the ladder keys, the draw and the splice all read this
      // one field.
      let seq = 0;
      const idOf = [];
      for (const b of blocksIn(dlines, own)) {
        const id = `B${seq++}`;
        idOf.push(id);
        drows.push({ id, kind: "para", grain: b.grain, name: b.name || null,
                     owner: b.up === undefined ? null : idOf[b.up],
                     from: b.from, to: b.to, text: b.text, was: b.text });
      }
      dparent = {};
      for (const r of drows)
        if (r.kind === "para" && r.owner) dparent[r.id] = r.owner;
      for (const c of h.children || [])
        drows.push({ id: `C${c.index}`, kind: "child", index: c.index,
                     level: c.level, cells: cellsOf(c) });
      const back = drows.findIndex((r) => r.id === was);
      dat = back === -1 ? 0 : back;
      dcol = null;
      drawDoc();
    }
    // The BODY a commit sends: the lines as they were, with each paragraph that
    // MOVED spliced back over its own range and each one DROP names taken out.
    // Bottom-up, so an earlier range is never moved by a later splice.  A
    // deletion eats the blank line that separated the block from the next one,
    // or a document would collect blank lines one delete at a time.
    function bodyText(drop) {
      const gone = drop || new Set();
      const out = dlines.slice();
      // ONE GRAIN SPEAKS FOR A RANGE.  A composite and its leaves cover the same
      // lines, so a commit must not splice both: a composite that MOVED, or that
      // is going, answers for everything inside it and its leaves are left out.
      // Which is why a reader flagging a list and one of its items still gets one
      // deletion rather than a corrupted body.
      // Spoken-for is TRANSITIVE down the ladder: a moved or going ancestor at
      // any rung answers for everything under it, so flagging an item and one
      // of its nested children is still one deletion.
      const spoken = new Set(drows.filter((r) => r.kind === "para"
        && (gone.has(r.id) || r.text !== r.was)).map((r) => r.id));
      const silenced = (r) => downersOf(r.id).some((o) => spoken.has(o));
      const paras = drows.filter((r) => r.kind === "para"
        && !silenced(r)).slice().reverse();
      for (const p of paras) {
        if (gone.has(p.id)) {
          const spare = p.to < out.length - 1 && String(out[p.to]).trim() === "";
          out.splice(p.from, p.to - p.from + (spare ? 1 : 0));
        } else if (p.text !== p.was) {
          out.splice(p.from, p.to - p.from, ...p.text.split("\n"));
        }
      }
      return out.join("\n");
    }
    // ONE DRAW, and it is the whole view.  Every element is a row of `#dlist'
    // wearing its KIND as a class, the one under point wearing `dat' — the
    // page's own selection, the same cursor language the table draws — and a
    // flagged element `dfl' beside it.
    //
    // A PART THE HEADLINE HAS NOT GOT RENDERS NOTHING, in every state: no
    // placeholders and no reserved gaps, so what a reader sees is the entry as
    // org spells it and what marks structure is the CURSOR alone.  Setting an
    // absent part is the COMMANDS' job — `t' and `:' at the element — rather
    // than a cell that has to be visible before it can be walked onto.
    // WHAT A STOP LOOKS LIKE: its kind as a class, `dat' on the one under point,
    // `dfl' on a flagged one.  A leaf is `d-item' and a composite `d-comp'
    // beside its own name, so a list and a block are stylable apart without the
    // draw knowing which is which.
    const dclass = (r, here) => `de d-${r.grain === "leaf" ? "item"
      : r.grain === "composite" ? `comp d-${r.name}` : r.kind}`
      + (here ? " dat" : "") + (dflags.has(r.id) ? " dfl" : "");
    // PARENT's children drawn inside BOX, each child recursing for its own —
    // the ladder on screen.  What no child claims is drawn as an INERT run
    // (`dg'), so every byte the parent covers is on screen exactly once, the
    // lens's rule one grain down at every rung.  A child with children draws
    // its HEAD — the lines ahead of its first child — as its own live text,
    // and hands the rest to the recursion.  Returns the index past the
    // subtree it consumed.
    function drawKids(box, parent, from, at0) {
      let at = at0 === undefined ? parent.from : at0, j = from;
      while (j < drows.length && drows[j].kind === "para"
             && drows[j].owner === parent.id) {
        const kid = drows[j];
        if (kid.from > at)
          part(box, "div", "dg", dlines.slice(at, kid.from).join("\n"));
        const kbox = part(box, "div", dclass(kid, j === dat));
        if (j === dat) dcursor = kbox;
        const under = j + 1 < drows.length && drows[j + 1].owner === kid.id;
        if (under) {
          const head = drows[j + 1].from;
          if (head > kid.from)
            drawPara(kbox, { from: kid.from, to: head,
                             text: dlines.slice(kid.from, head).join("\n") });
          // The recursion's inert tracking starts where the head ENDED, or the
          // head's lines would be drawn twice — once live, once as a gap.
          j = drawKids(kbox, kid, j + 1, head);
        } else { drawPara(kbox, kid); j += 1; }
        at = kid.to;
      }
      if (at < parent.to)
        part(box, "div", "dg", dlines.slice(at, parent.to).join("\n"));
      return j;
    }
    function drawDoc() {
      const list = el("dlist");
      list.textContent = "";
      dcursor = null;
      for (let i = 0; i < drows.length; i += 1) {
        const r = drows[i];
        const here = i === dat;
        const row = part(list, "div", dclass(r, here));
        if (here) dcursor = row;
        // A COMPOSITE IS DRAWN ONCE, with its leaves INSIDE it: the walk has two
        // stops over one range and the reader must see one list.  Its leaves are
        // the rows straight after it, so the draw walks them here and the outer
        // loop steps past what it took.  What no leaf claims is drawn as an INERT
        // run — the `#+begin_' and `#+end_' lines, the blank line between two
        // items, a lead-in the list's own listOpener did not take — so every byte the
        // composite covers is on screen exactly once, the lens's rule one grain
        // down.
        if (r.grain === "composite") {
          i = drawKids(row, r, i + 1) - 1;
        } else if (r.kind === "para") drawPara(row, r);
        else drawCells(row, r, here);
      }
      keepInView(dcursor);
      placeEdit();
    }
    // THE CURSOR STAYS IN VIEW, the TABLE's own discipline over the pane this
    // page owns: `#mdoc' scrolls inside the sheet's bound, so an element below
    // the fold is reachable by `n' and invisible without this.
    //
    // `scrollIntoView' IS LEGITIMATE HERE and forbidden on the table's rows,
    // which belong to the renderer and its scroller and page, where reaching in
    // that way would be this page working around an interface it has.  The
    // document is the SHELL's — its rows, its scroller — and the suite keeps the
    // distinction by counting: exactly one `scrollIntoView' in the page, this
    // one, so a second would have to be a reach into something it does not own.
    //
    // THE BAND IS THE ELEMENT'S OWN MARGIN.  `block:"nearest"' honours
    // `scroll-margin', so `.de' carrying `scroll-margin-block' is the whole of
    // the scrolloff: the platform holds the cursor three lines clear of either
    // edge, in both directions, and stepping down past the band scrolls exactly
    // far enough to keep it.  An element already inside the band is left where it
    // is, so the pane never re-centres under a reader walking through it — which
    // keeps the movement code ONE CALL, no measuring, no `scrollTop' arithmetic,
    // and the same band whether the pane was last moved by a key or by a wheel.
    // No easing: the pane is small and a smooth scroll would still be running
    // when the next `n' lands.
    function keepInView(row) {
      if (row && typeof row.scrollIntoView === "function")
        row.scrollIntoView({ block: "nearest" });
    }
    // A headline line: the four cells side by side, the one under point marked.
    // A CHILD is the same line indented by its own level.  The state cell takes
    // its badge hue, so a keyword reads here as in the table.
    //
    // THE STARS, ORG-CLEANED: every star but the LAST rendered as a space, the
    // way `org-hide-leading-stars' with `org-startup-indented' draws them.  The
    // indentation IS the outline, which is why child lines carry no padding of
    // their own.  Depth is org-indent's arithmetic — TWO spaces a level, so a
    // child's star sits at the column the parent's BODY starts at — and it is
    // RELATIVE to the entry the sheet stands on, so materializing into a child
    // makes THAT line the root.
    //
    // IT IS CHROME RATHER THAN A CELL: absent from `r.cells', so `f'/`b' walk
    // past it and `dcol' indexes the cells alone.  The hidden stars are SPACES,
    // so the only ink in the prefix is the last star's.
    const dstars = (level) =>
      " ".repeat(Math.max(0, 2 * (level - docLevel()))) + "* ";
    const docLevel = () => (editing && editing.level) || 1;
    // The cells a headline line actually HAS, which is the whole of what it
    // draws and the whole of what `f'/`b' stop on: an absent part is not a stop,
    // so a bare title is one stop and nothing has to be shown before it can be
    // reached.  `dcol' indexes THIS list rather than the model's four.
    const shown = (r) => (r.cells || []).filter((c) => c.val);
    // A paragraph's own text, with the references in it drawn as references.
    // The element's file range is what the links are intersected against, and it
    // is the same range `o' scopes by — so what a reader SEES marked in an
    // element is exactly what `o' there will find.
    function drawPara(row, r) {
      const at = elementSpan(r);
      const box = part(row, "div", "dp");
      if (at) drawText(box, r.text, at[0], null); else box.textContent = r.text;
    }
    function drawCells(row, r, here) {
      part(row, "span", "ds", dstars(r.kind === "child" ? r.level : docLevel()));
      shown(r).forEach((c, j) => {
        const cell = part(row, "span", `dc dc-${c.key}`
          + (here && j === dcol ? " don" : ""));
        // The TITLE is the one cell that can hold a reference, and the server
        // says where it starts (`titleAt') because only it has the sub-span.
        // A CHILD's title is left as text: its cell is a line of another
        // entry's outline rather than this document's own bytes, and no offset
        // for it is sent.  EXACTLY ONE of the two paths writes the cell:
        // presetting the text and then appending segments left the raw
        // brackets standing beside the description in a real DOM — the
        // harness holds textContent and children apart, so only a browser
        // could see the double.
        if (c.key === "title" && r.kind === "head"
            && editing && typeof editing.titleAt === "number")
          drawText(cell, c.val, editing.titleAt, null);
        else cell.textContent = c.val;
        if (c.key === "state") cell.style.color = badgeColor(c.val);
      });
    }
    // MOVEMENT IS TWO AXES, the table's own habit read into the document:
    // `n'/`p' walk SIBLINGS at the current grain and never dive — a list is ONE
    // stop however many items it holds, so holding `n' skims the document at
    // reading grain — and `f'/`b' move the GRAIN itself, finer and broader.
    // `l'/`h' and the horizontal arrows stay the within-grain cell walk,
    // walking off either end into the whole-element look rather than bumping,
    // which is the rule `moveCol' keeps over the table.
    const colStep = (k) => (k === "<right>" || k === "l" ? 1
                          : k === "<left>" || k === "h" ? -1 : 0);
    const grainStep = (k) => (k === "f" ? 1 : k === "b" ? -1 : 0);
    const dcells = (r) => (r && (r.kind === "head" || r.kind === "child")
                            ? shown(r).length : 0);
    // Siblings at the cursor's grain: a leaf steps only to its owner's other
    // leaves — they are contiguous behind their whole, so the neighbour row
    // decides — and the element grain steps over every leaf run whole.  Both
    // clamp at their ends, silently, the way the old walk clamped at the
    // document's.
    function docStep(step) {
      if (!drows.length) return;
      const cur = drows[dat];
      let i = dat + step;
      if (cur && cur.grain === "leaf") {
        // The sibling is the next row sharing MY owner; every LEAF between two
        // siblings is deeper in the same parent's subtree — my descendants going
        // forward, a sibling's coming back — because the emission order puts a
        // COMPOSITE between any two parents' runs.  So the scan steps past
        // leaves, breaks on the owner match, and clamps at the first non-leaf:
        // the edge of the parent's run, structurally.
        while (i >= 0 && i < drows.length) {
          const kin = drows[i];
          if (kin.grain !== "leaf") { drawDoc(); return; }
          if (kin.owner === cur.owner) break;
          i += step;
        }
        if (i < 0 || i >= drows.length) { drawDoc(); return; }
        dat = i;
      } else {
        while (i >= 0 && i < drows.length && drows[i].grain === "leaf") i += step;
        if (i < 0 || i >= drows.length) { drawDoc(); return; }
        dat = i;
      }
      if (!dcells(drows[dat])) dcol = null;
      dgrain = dcol !== null ? "cell"
             : drows[dat].grain === "leaf" ? "leaf" : "element";
      drawDoc();
    }
    // The grain keys.  `f' on a composite enters its leaves; on a headline it
    // enters the cells, which ARE that line's finer grain; at the finest it
    // refuses with an echo — finer that does not exist should say so.  `b' is
    // the mirror: a cell point broadens to the whole line in one press
    // (whatever the column — `l'/`h' keep the walk-off spelling), a leaf to
    // its composite whole, and the element grain is the floor: a no-op with an
    // echo, never a close, since going OUT of the sheet is `DEL''s and the
    // movement/context split dies the moment `b' can shut something.
    // A row's DIRECT children: the rows owning it, counted for the echo and
    // probed for the descent.
    const kidsOf = (id) =>
      drows.filter((r) => r.kind === "para" && r.owner === id).length;
    function docFiner(k) {
      const say = keySaid(k), r = drows[dat];
      if (!r) return;
      const kids = r.kind === "para" ? kidsOf(r.id) : 0;
      if (kids) {
        // The first child immediately follows its parent in emission order,
        // whether the parent is the composite or an item one rung up.
        dat += 1; dgrain = "leaf"; drawDoc();
        say(`grain-finer (${r.name || "item"} 1/${kids})`);
      } else if (dcells(r)) { moveDocCol(k, 1); }
      else if (r.grain === "leaf") say("grain-finer (at the finest)");
      else say("grain-finer (nothing finer here)");
    }
    function docBroader(k) {
      const say = keySaid(k), r = drows[dat];
      if (!r) return;
      if (dcol !== null) {
        dcol = null; dgrain = "element"; drawDoc();
        say("grain-broader (element)");
      } else if (r.grain === "leaf") {
        // One rung up: the IMMEDIATE owner, an item or the composite, never a
        // scan to the nearest non-leaf — the ladder is climbed a step at a time.
        const i = drows.findIndex((x) => x.id === r.owner);
        if (i === -1) { say("grain-broader (at the element grain)"); return; }
        dat = i;
        dgrain = drows[i].grain === "leaf" ? "leaf" : "element";
        drawDoc();
        say(`grain-broader (${drows[i].name || (drows[i].grain === "leaf"
                              ? "item" : drows[i].kind)})`);
      } else say("grain-broader (at the element grain)");
    }
    function moveDocCol(k, step) {
      const say = keySaid(k), n = dcells(drows[dat]);
      if (!n) { say("next-column (no cells in this element)"); return; }
      const want = dcol === null ? (step > 0 ? 0 : n - 1) : dcol + step;
      dcol = want < 0 || want >= n ? null : want;
      dgrain = dcol === null ? "element" : "cell";
      drawDoc();
      // The landing is read back off the cursor rather than off WANT, which is
      // the table's own rule: a column outside the element is no column at all
      // and the whole-element look is a real move rather than a swallowed key.
      say(`next-column (${dcol === null ? "element mode" : shown(drows[dat])[dcol].key})`);
    }
    // WHERE AN ELEMENT SITS IN THE FILE, in the offsets `/links' answers in, so
    // `o' can ask which of the row's links are inside THIS element rather than
    // matching its text — which a URL the entry spells twice would fool.
    //
    // Derived from what the answer already carries rather than from a field added
    // for it.  The BODY is the subtree with the three regions lifted out, and all
    // three sit ABOVE the paragraphs (a planning line is the line under the
    // title, both drawers follow it, and the logbook scan stops at the first
    // child), so every body offset past the title line is displaced by ONE
    // constant, which is what the two lengths differ by; the title line itself
    // precedes them and is displaced by nothing.  A CHILD's extent is the
    // SERVER's and is the one this cannot derive, its own subtree running past
    // the body the sheet is holding. OFFSETS ARE IN CHARACTERS.  Every span the
    // server answers in is a CHAR offset into the file (docs/invariants.md), and
    // JavaScript's `length' and `slice' count UTF-16 units — so ONE astral
    // character anywhere above a link (an emoji in a title, a rare CJK glyph in a
    // paragraph) put every element extent and every link segment one out and cut
    // the link's text in half. Both readings live here, and every offset this
    // pane computes goes through them.
    const chars = (s) => Array.from(String(s));
    const clen = (s) => chars(s).length;
    const cslice = (s, a, b) => chars(s).slice(a, b).join("");
    const bodyShift = () => clen(editing.org || "") - clen(editing.body || "");
    const charOf = (line) =>
      dlines.slice(0, line).reduce((n, l) => n + clen(l), 0) + line;
    // THE ROW'S LINKS, held from the materialize so the DISPLAY can use them.
    // `/links' is the server's one scan of the subtree and the only authority on
    // where a link is and what it shows: this page has no bracket grammar and
    // must not grow one, so what it does with the answer is arithmetic —
    // intersect the ranges into an element's own coordinates and draw segments.
    let dlinks = [];
    // WHICH OF THEM ARE INSIDE A RANGE.  One predicate, two readers: the draw
    // below and `o', which is where it was written.
    const linksIn = (at, links) => (links || dlinks).filter((l) =>
      l.span && l.span[0] >= at[0] && l.span[1] <= at[1]);
    // TEXT WITH ITS LINKS DRAWN, into INTO.  AT is the file offset TEXT starts
    // at, which turns a link's file range into an offset in this string.
    //
    // DISPLAY IS THE DESCRIPTION, SOURCE IS THE FILE — org's own model, and the
    // table's: `[[T][D]]' shows `D', `[[T]]' shows `T', a bare URL shows itself.
    // The shown text is the server's `desc' verbatim (`Glance.Query.linkShown'),
    // so the rule is spelled once, on the side that did the scan.  `RET' opens
    // the RAW org, brackets and all — the display never becomes the source, so
    // editing is always over what the file says.  SPAN-DRIVEN, never
    // search-driven: one URL written three times is three ranges, each drawn
    // where it stands.
    function drawText(into, text, at, links) {
      const n = clen(text);
      let cut = 0;
      for (const l of linksIn([at, at + n], links)) {
        const a = l.span[0] - at, b = l.span[1] - at;
        if (a < cut) continue;
        if (a > cut) part(into, "span", "dt", cslice(text, cut, a));
        part(into, "span", "dl", l.desc);
        cut = b;
      }
      if (cut === 0) { into.textContent = text; return; }
      if (cut < n) part(into, "span", "dt", cslice(text, cut));
    }
    function elementSpan(r) {
      const at = (editing.span || {}).start;
      if (at === undefined || !r) return null;
      if (r.kind === "child") return r.span ? [r.span.start, r.span.end] : null;
      if (r.kind === "head") return [at, at + charOf(1)];
      if (r.kind !== "para") return null;
      const shift = at + bodyShift();
      return [shift + charOf(r.from), shift + charOf(r.to)];
    }
    // `o' IN THE DOCUMENT IS THE TABLE'S `o' AT ONE GRAIN FINER: there it
    // follows the ROW's links (the whole subtree), here the ELEMENT's.  One
    // answer either way — `/links' is asked for the row and the element's own
    // extent is what narrows it — and one gesture: none says so, one opens, and
    // several raise the popup this page already has, with its own `o' and its
    // own `RET' edit inside it unchanged.
    function openHere() {
      const r = drows[dat], b = docBinding("org-glance-overview:open");
      const at = elementSpan(r);
      if (!at) { said(b, "nothing to open here"); return; }
      // The links are already in hand — they rode the materialize — so the
      // element's `o' asks the server nothing; the digest a popup edit pins
      // is the sheet's own, the same file's.
      const links = linksIn(at);
      followLinks(b, editing.id, { digest: editing.digest, links }, links);
    }
    // What the echo and the prompts call the entry the sheet is standing on.
    const docTitle = () =>
      ((editing && editing.cells && editing.cells.title) || (editing || {}).id || "");
    // A binding this page can hand `said' and `fire' where no keymap row raised
    // the write: the document's keys are its own listener's, the way the panel's
    // and the popups' are, so the command NAME travels with the call.
    const docBinding = (command, seq) => ({ seq: seq || "RET", command });
    // RET, BY KIND, and that is the whole surface: a child materializes, a
    // paragraph opens as text, a property and the title open as fields, a
    // planning row asks for a date, the state and tag cells raise the page's
    // own palettes over the row, and the headline LINE itself opens its title.
    function docEnter() {
      const r = drows[dat];
      if (!r) return;
      if (r.kind === "child") { into(r.index); return; }
      if (r.kind === "para") { openEdit(DPARA, r); return; }
      headEnter(r);
    }
    // The headline's own cells are the ROW's, and a row is what `/command'
    // addresses: a child headline has no row id, so its cells are read-only here
    // and the echo says which key reaches the entry that owns them.  Its
    // planning, its drawer and its body are all still editable, through the lens
    // that materialized it.
    function headEnter(r) {
      // The cell at point, and a cursor can outlive the cells it was taken on: a
      // stash put back over a headline that has since lost one names a column
      // that is not there, so the cell is READ rather than assumed.
      const c = dcol === null ? null : shown(r)[dcol];
      if (editing.child !== null) {
        echo(`RET → a child's ${c ? c.key : "title"} is not settable yet — DEL opens its parent`);
        return;
      }
      if (c && c.key === "state") { stateHere(); return; }
      if (c && c.key === "tags") { tagsHere(); return; }
      // The WHOLE LINE's edit is its TITLE: state and tags have popups, the
      // priority ring is pressed, and the raw bytes are the server's — so RET
      // at the element grain and RET on the title cell are one door, and a
      // headline that has no title yet opens it empty (`set-title' inserts
      // where nothing was).
      if (!c || c.key === "title") {
        const t = shown(r).find((x) => x.key === "title");
        openEdit(DTITLE, { id: "CELL:title", val: t ? t.val : "" });
        return;
      }
      // A RING OF THREE IS PRESSED, NOT PICKED: the two keys answer faster than
      // any list a cell could raise, so this stays a refusal — one that now
      // names the keys rather than an absence.
      echo("RET → priority cycles on S-<up>/S-<down>");
    }
    // The state cell raises the value palette this page already has, targeted at
    // the row the sheet is on rather than at the table's selection: the offer is
    // `/keywords'' answer for THAT row, so what is shown and what a write takes
    // are one answer here as everywhere else.
    // The two element keys are the HEADLINE's, and a CHILD has no row id for a
    // `/command' to name, so they are refused there the way its cells are.
    function atElement(act) {
      const r = drows[dat];
      if (!r || (r.kind !== "head" && r.kind !== "child"))
        { echo("the headline line takes this — n/p to it"); return; }
      if (r.kind === "child" || editing.child !== null) {
        echo("a child is not settable yet — DEL opens its parent");
        return;
      }
      act();
    }
    // The priority ring over the entry the sheet is standing on: one row, so no
    // grouping is owed — the table's own `cyclePriority' is the general case and
    // this is it at a set of one, spelled here because the value comes off the
    // ANSWER's cells rather than off a table row this page may not be showing.
    function cycleHere(step) {
      const b = docBinding(step > 0 ? "priority-up" : "priority-down",
                           step > 0 ? "S-<up>" : "S-<down>");
      const want = cycled(priorityIn((editing.cells || {}).priority), step);
      fire(b, "set-priority", [editing.id], { priority: want },
           want ? `[#${want}]` : EMPTY);
    }
    const stateHere = () =>
      docTargets(docBinding("org-glance-overview:todo"), "set state", askState);
    // And the tag cell raises the tags popup, over the same one row.  It is the
    // page's `:' with the set settled: a sheet is open on ONE entry, so there is
    // no marked set to inherit and no question about which rows it means.
    const tagsHere = () =>
      docTargets(docBinding("org-agenda-set-tags"), "tags", askTags);
    // DEL IS UP.  In a child the sheet re-materializes the entry above it — the
    // server's own `parent', null being the row — and lands the cursor back on
    // the child it came out of.  At the top there is nothing above the row, so
    // the key is the sheet's door.
    // A RE-MATERIALIZE of the sheet's own row under CHILD, with K run over the
    // entry it was standing on and the fresh answer.  Four presses ask for one
    // (`DEL' up, `RET' into a child, the re-read a commit lands on, and `C-c \''
    // swapping the shape) and each owes the SAME two guards: the sheet may have
    // moved on while the read was out, in which case the answer is dropped, and
    // a read that never came back is the sheet's own `stuck'.
    function reread(child, k) {
      if (!editing) return;
      const h = editing;
      headline(h.id, child).then((fresh) => { if (editing === h) k(h, fresh); })
        .catch((e) => stuck(subtreeSheet, e.message));
    }
    function docUp() {
      if (!editing) return;
      if (editing.child === null) { leaveSheet(); return; }
      const up = editing.parent;
      reread(up === null ? undefined : up, (h, fresh) => {
        show(fresh, raw);
        const back = drows.findIndex((r) => r.kind === "child" && r.index === h.child);
        if (back !== -1) { dat = back; drawDoc(); }
        echo(`DEL → org-glance-overview:up (${docWhere(fresh)})`);
      });
    }
    // And RET on a child is DOWN: the sheet re-materializes INTO it, which is the
    // same route under a `child=' the server handed over.  The subtree the lens
    // is over moves; the row, the file and the digest do not.
    function into(index) {
      reread(index, (_h, fresh) => {
        show(fresh, raw);
        echo(`RET → org-glance-overview:materialize (${docWhere(fresh)})`);
      });
    }
    const docWhere = (h) => (h.path || []).slice(-1)[0] || h.id;
    // WHAT A SUBTREE WRITE ANSWERS, shared by the three that make one: a 200
    // re-pins the digest and hands the caller its own line, and under that is ONE
    // ladder — a moved file waits for a keystroke at `conflict', a refused
    // planning entry names its field through `stuck', a request that never landed
    // says why.  It reports whether the write LANDED, which is what the sheet's
    // own flush resolves to and what decides a re-read here.
    function landed(h, onOk) {
      return (a) => {
        if (a.status === 200) {
          h.digest = a.body.digest;
          sync("synced");
          onOk(a);
          return true;
        }
        // A refused planning entry is a 409 like a moved file, and it waits for a
        // keystroke the same way — but it names the field rather than the file, so
        // it goes through `stuck' and says so.
        if (a.status === 409 && a.body.reason !== "planning") sync("conflict");
        else stuck(subtreeSheet, a.body.error || `sync failed (${a.status})`);
        return false;
      };
    }
    // THE COMMIT, and every element of the SUBTREE goes through it: one `POST
    // /headline' carrying the body, the drawer and the planning line as the model
    // holds them, pinned to the digest this sheet was handed.  DROP names the
    // paragraphs a deletion is taking out, those leaving no trace in the model to
    // read afterwards.  The answer re-pins the digest and the sheet
    // re-materializes off it, so the model is the server's reading of what was
    // just written rather than this page's guess at it.
    const commitDoc = (what, drop) =>
      commitDocWith(bodyText(drop), () => { if (what) echo(`RET → ${what}`); });
    // ORG'S CHECKBOX, at the stop under point.  The box lives on an item's
    // FIRST line (`- [ ] text', numbered items included), so the read is one
    // regex over it: the state character, or null where the stop opens with no
    // box — a paragraph, a table line, a headline.  The toggle is org's own
    // `C-c C-c' rule: `[ ]' checks, `[X]'/`[x]' clears, and the partial `[-]'
    // a parent inherits from its children checks.
    const CHECKBOX = /^(\s*(?:[-+*]|\d+[.)])\s+)\[( |X|x|-)\]/;
    const checkboxAt = (r) =>
      r && r.kind === "para"
        ? (CHECKBOX.exec((r.text || "").split("\n")[0]) || [])[2] ?? null
        : null;
    // The write is the paragraph edit's own: the stop's text with the box
    // respelled, spliced over its lines by `commitDocWith', every other byte
    // where it was.  SAY carries the binding that fired, `SPC' and `C-c C-c'
    // both reaching here.
    function toggleCheckbox(b) {
      const r = drows[dat];
      const was = checkboxAt(r);
      if (was === null) { said(b, "no checkbox here"); return; }
      const now = was === " " ? "X" : was === "-" ? "X" : " ";
      r.text = r.text.replace(CHECKBOX, `$1[${now}]`);
      // The box flips ON SCREEN with the press: the model is the intent, and
      // the write's own receipt decides whether it stands (`landed').
      drawDoc();
      commitDocWith(bodyText(), () => said(b, `[${now}]`));
    }
    // The sheet re-read, in place: the same entry, the fresh parts, the cursor
    // kept by id.  It is what a commit lands on and what a socket frame naming
    // this row asks for — the watch is the channel a `/command' write comes back
    // through, exactly as it is for the table.
    // THE STORE LAGS THE WRITE IT ANSWERS FOR.  `GET /headline' serves the
    // STORE's copy, and the watch that refreshes it is a debounce away — so a
    // re-read fired by our own 200 can hand back the PRE-write subtree under
    // the PRE-write digest.  Taking that answer reverted the pane to what the
    // file just stopped saying and POISONED the sheet's pin (the next write
    // 409s against a digest the file has already left) — and a body-only edit
    // emits NO frame, so nothing ever corrected it.  The 200's digest is the
    // truth of what landed (`landed' pinned it), so an answer under any OTHER
    // digest is dropped whole: the model — which the write was built from and
    // the 200 proved — stands, redrawn, and ONE delayed retry fetches the
    // server's canonical reading once the watch has caught up.  Still stale
    // then (or moved further, a foreign edit): the model stands for good, and
    // the next write's own lock says whatever there is to say.
    function reload() {
      if (!editing) return;
      const h = editing;
      const read = (retry) => headline(h.id, h.child).then((fresh) => {
        if (editing !== h) return;
        if (fresh.digest !== h.digest) {
          if (retry) setTimeout(() => { if (editing === h) read(false); }, 300);
          drawDoc();
          return;
        }
        editing = fresh;
        fill(fresh);
        sync("synced");
      }).catch((e) => stuck(subtreeSheet, e.message));
      read(true);
    }
    // THE EDIT OVERLAY, ONE mechanism over four surfaces.  The renderer owns its
    // rows and rewrites them as it scrolls, so an edit cannot live inside one:
    // the fields sit OVER the table, anchored to the row the cursor is on.  The
    // document opens a headline's title over the title text, or a paragraph as
    // text; the tags popup opens one cell as a field over itself; the link
    // popup opens two.  Everything else is the same — the class that shows the box, the
    // anchor, the blur on the way out — so a SHAPE says what differs
    // (`DTITLE', `DPARA', `TROW', `LROW') and this holds the gesture.
    //
    // SNAPSHOTTED AT OPEN, the property this shape exists to have.  No key can
    // move the cursor while a row is open, but a MOUSE CLICK can, and a commit
    // that re-read the cursor would write the text typed for one row into
    // whichever row the reader landed on.  `edit' keeps what was opened over and
    // a commit is handed it, so every surface has the guard the tags rename was
    // written with.
    //
    // One `edit' for all four, no two being up at once: the document needs the
    // subtree sheet open, and each popup is raised over the table alone and
    // counts as `typing()' while it stands, so neither can raise the other.
    // `dediting()', `dparaing()', `renaming()' and `lediting()' ask WHOSE it is.
    let edit = null;
    function openEdit(o, row) {
      edit = { o, row };
      el(o.box).className = "on";
      o.fill(row);
      // The renderer stamps `tv-sel' on its own frame, so a row selected in
      // THIS tick has no marked element yet: `+' would measure the row the
      // cursor was on before it.  One frame later there is one.
      soon(placeEdit);
      o.focus(row);
    }
    // SHUT MINE, and O is which surface is asking.  The two shapes share one
    // `edit', and a caller naming its own keeps the sharing from reaching across:
    // the tags popup CAN stand over an open materialize sheet — clicking the
    // sheet's own chrome blurs its textarea, `typing()' goes false and every
    // `table' row is live again, the same hole `openSettings' refuses by hand —
    // so an unscoped shut would let the sheet's own `fill' and `shut' silently
    // cancel an open tag rename.  Naming the shape restores exactly the isolation
    // the two hand-written shutters had, for one argument. TAB INSIDE AN OPEN
    // EDIT hops that edit's own fields, every shape declaring them already
    // (`fields'), so the hop reads the list rather than naming a pair — a third
    // field then works everywhere instead of leaving three copies silently wrong.
    // It WRAPS, which makes S-TAB the same line, and a focus outside the list
    // lands on the first, where the two hand-written pairs put it.
    function hop() {
      const ids = edit.o.fields;
      const at = ids.findIndex((id) => el(id) === active());
      el(ids[(at + 1) % ids.length]).focus();
    }
    function shutEdit(o) {
      if (!edit || edit.o !== o) return;
      el(edit.o.box).className = "";
      for (const id of edit.o.fields) el(id).blur();
      edit = null;
    }
    // ESC OVER AN OPEN EDIT, wherever it is open: the overlay goes and the thing
    // under it stands, holding the text it was opened on.  WHAT names it in the
    // pill — an element, a row, a link, a tag — and the SHAPES are the caller's
    // own, the scoping `shutEdit' asks for; the document names two, having a
    // second box for a paragraph.  One sentence, so a fifth surface cannot word
    // the same event differently.
    const cancelEdit = (what, ...shapes) => {
      for (const o of shapes) shutEdit(o);
      echo(`ESC → keyboard-quit (${what} unchanged)`);
    };
    // Where the overlay sits: over the row the renderer has selected.  Its
    // GEOMETRY is the only thing this page reads out of a mount's own DOM, and it
    // reads nothing about the row but where it is.
    //
    // A `cells' shape narrows to a RUN of columns as well, and it names them BY
    // KEY — the tags popup edits `["title"]', the link popup `["title", "url"]'
    // (the derived type column being the one it may not open), and the document's
    // two name none and take the whole element.  The keys are resolved against
    // the shape's OWN column list, the one the SERVER declared
    // (`Glance.Query.linkColumns', `tagColumns') and this page embeds, so
    // reordering those columns moves the overlay with them and inserting one
    // ahead of the run costs nothing.  A key no column carries resolves to
    // nothing and the placement is a no-op — a box left where it was rather than
    // a box over the wrong cells.  The GUTTER `flags: true' puts in front is
    // skipped by the class the renderer already stamps, so the resolution counts
    // the popup's own columns and nothing of its chrome. WHERE THE ROW IS, and a
    // shape says how to find it.  Three of the four surfaces are table-view
    // mounts and read the renderer's own selected row through the handle's
    // published root — the mount publishes it, so the one geometry read this page
    // makes goes through a published door — and the document is no mount at all,
    // so it names the element under point directly. One reader either way, so the
    // geometry, the cell run and the resize stay one implementation.
    const anchorOf = (o) => {
      if (o.anchor) return o.anchor();
      const m = o.mount();
      return m ? m.el.querySelector("tbody tr.tv-sel") : null;
    };
    function placeEdit() {
      if (!edit) return;
      const o = edit.o;
      const tr = anchorOf(o);
      // A page with no layout — the suite's, and a sheet still `display:none'
      // while it is being filled — measures nothing and leaves the overlay
      // exactly where it was put.
      if (!tr || typeof tr.getBoundingClientRect !== "function") return;
      const span = o.cells && cellSpan(o.cells, o.cols);
      if (o.cells && !span) return;
      const tds = span && [...tr.querySelectorAll("td:not(.tv-box)")];
      const from = tds && tds[span[0]], to = tds && tds[span[1]];
      if (o.cells && !(from && to)) return;
      const pane = el(o.pane);
      if (typeof pane.getBoundingClientRect !== "function") return;
      const a = tr.getBoundingClientRect();
      const b = pane.getBoundingClientRect();
      const s = el(o.box).style;
      // FROM THE PANE'S PADDING BOX, AND WITH ITS SCROLL.  An absolutely
      // positioned child is placed against its containing block's PADDING box and
      // scrolls with the content rather than with the viewport, so a pane
      // carrying a border or a scroll offset needs both back: `clientTop' is that
      // border, `scrollTop' that offset.  `#mprops' has neither, which is why the
      // bare delta was right where this was written and wrong the moment `#mdoc'
      // — bordered, padded and scrolling — reused it.
      s.top = `${a.top - b.top - pane.clientTop + pane.scrollTop}px`;
      s.height = `${a.height}px`;
      // A TIGHT shape covers its anchor's own box: left off the anchor, right
      // off `edge''s element where the shape names one (the tags cell — the
      // dress the edit leaves standing), else the anchor's parent line.  CSS
      // keeps the floor (`min-width'), so a short title still takes typing.
      if (o.tight) {
        const e = o.edge && o.edge();
        const stop = e && typeof e.getBoundingClientRect === "function"
          ? e.getBoundingClientRect().left
          : tr.parentElement
              && typeof tr.parentElement.getBoundingClientRect === "function"
            ? tr.parentElement.getBoundingClientRect().right
            : b.right;
        s.left = `${a.left - b.left}px`;
        s.width = `${stop - a.left}px`;
        return;
      }
      if (!o.cells) return;
      const l = from.getBoundingClientRect(), rt = to.getBoundingClientRect();
      s.left = `${l.left - b.left}px`;
      s.width = `${rt.right - l.left}px`;
    }
    // WHERE A RUN OF NAMED COLUMNS SITS: the leftmost and rightmost of KEYS as
    // indices into COLS, or null where any of them names no column there.  The
    // RUN IS THE COLUMNS' ORDER rather than the shape's, a box being drawn from
    // one edge to the other and a shape spelling its keys the other way round
    // meaning the same two cells.  Pure and order-only, so the answer is a
    // property of the two lists and of nothing else on the page, which lets the
    // suite check it against the server's own column declaration rather than
    // against a copy of it.  A declaration rather than a `const', so a direct
    // `eval' of this glue leaks it the way it leaks `whichKeys'.
    function cellSpan(keys, cols) {
      const at = (keys || []).map((k) => (cols || []).findIndex((c) => c.key === k));
      if (!at.length || at.some((i) => i < 0)) return null;
      return [Math.min(...at), Math.max(...at)];
    }
    // The overlay is anchored to a row's box, so the window resizing has to move
    // it — once, here, for every surface, since one `placeEdit' answers for
    // whichever is open.  A mount's own scrolling is registered with it.
    window.addEventListener("resize", placeEdit);
    el("mdoc").addEventListener("scroll", placeEdit, true);
    // THE EDIT SHAPES, both on the page's own overlay mechanism (`openEdit'), so
    // a document element, a panel row, a tag and a link are edited alike — the
    // snapshot at open, the blur on the way out, ESC through the keymap's
    // `cancel'.  What differs is the box: a pair of fields for a key and a value,
    // and a textarea for a paragraph, which is text and wants its newlines.
    // `anchor' is the one thing a shape declares here that a mount's does not:
    // the document is no mount, so it names the element under point rather than
    // the renderer's `tv-sel' row.
    const docElAt = () => dcursor;
    // THE TITLE EDITS IN PLACE, and the headline keeps its dress: one field
    // laid over the TITLE CELL's own box — `tight', so `placeEdit' reads the
    // cell for both axes — with the stars, the state badge and the tags still
    // on screen around it.  The right edge stops where the TAGS begin
    // (`edge'), else at the row's end; a headline with no title cell yet has
    // no box to read, so the anchor falls back to the whole line, which is
    // where an inserted title will go.
    const dTitleAt = () =>
      (dcursor && dcursor.querySelector && dcursor.querySelector(".dc-title"))
        || dcursor;
    const DTITLE = {
      box: "dtitle", pane: "mdoc", fields: ["dtin"],
      mount: () => null, anchor: dTitleAt, tight: true,
      edge: () =>
        dcursor && dcursor.querySelector && dcursor.querySelector(".dc-tags"),
      fill: (r) => { el("dtin").value = r.val; },
      focus: () => el("dtin").focus(),
    };
    const DPARA = {
      box: "dpara", pane: "mdoc", fields: ["dtext"],
      mount: () => null, anchor: docElAt,
      fill: (r) => { el("dtext").value = r.text; },
      focus: () => el("dtext").focus(),
    };
    const dediting = () => !!edit && edit.o === DTITLE;
    const dparaing = () => !!edit && edit.o === DPARA;
    const docOpen = () => dediting() || dparaing();
    // The surface is UP whenever a subtree sheet is on screen, in EITHER shape.
    // The structured document holds the keys with NOTHING focused, the way the
    // panel's nav does, so `typing()' has to count it; RAW MODE counts with it,
    // a textarea being BLURRABLE — clicking the sheet's own header does it — and
    // a surface that stopped counting the moment a reader touched its chrome left
    // every `table' row live under an open sheet, `d' among them, which archives
    // the row behind it.  WHAT THAT COSTS IS `q': it is scope `table', so with
    // either sheet open it is dead and `quitWindow''s `editing ? leaveSheet()'
    // arm is unreachable, the sheet's doors being ESC and the backdrop.  Over the
    // table `q' says there is no window to quit, now the whole of what it says.
    const docHolds = () => editing !== null;
    // The commit: the ELEMENT takes the text its fields are holding, and the
    // write goes out.  The element is the one the overlay OPENED over, never the
    // one point is on now — the snapshot `openEdit' keeps, and what a mouse click
    // under an open edit would otherwise redirect.
    // B is the binding that fired, since TWO keys commit an open element —
    // `C-x C-s' and org's `C-c C-c' — and the echo names the command that ran.
    // Absent, the caller is `RET' inside the overlay, which spells its own line.
    function commitDocEdit(b) {
      const spoke = (what) => (b ? said(b, what) : echo(`RET → ${what}`));
      if (!edit) return;
      const r = edit.row;
      if (edit.o === DPARA) {
        const text = el("dtext").value;
        shutEdit(DPARA);
        if (text === r.text) { spoke("paragraph unchanged"); return; }
        r.text = text;
        commitDoc("paragraph written");
        return;
      }
      const val = el("dtin").value;
      shutEdit(DTITLE);
      retitle(val);
    }
    // The title is a CELL, so it is a command rather than a subtree write: the
    // span math replaces the title's own characters and the keyword in front of
    // it and the tags behind it keep their bytes.  The refusals — an empty title,
    // a second line — are the server's, and they are the whole request's.
    function retitle(val) {
      fire(docBinding("org-glance-overview:rename"), "set-title", [editing.id],
           { title: val }, `retitled ${JSON.stringify(val.trim())}`);
    }
    const cancelDocEdit = () => cancelEdit("element", DTITLE, DPARA);
    // The sheet is ONE surface with two panes, so it is one entry in `SURFACES'
    // and ESC puts back whichever pane's edit is open.  Below that the ladder
    // falls through to the sheet itself, which is where it always did.
    const sheetOpen = () => docOpen() || pediting();
    const cancelSheetEdit = () => (pediting() ? cancelRow() : cancelDocEdit());
    // DELETION IS KIND-AWARE, and over this pane the kind it reaches is the
    // PARAGRAPH: the table's own gesture, over the document's own flags — `d'
    // flags, a second `d' — or `D' — takes every flagged element, `u' takes a
    // flag off.  A paragraph is spliced OUT of the body; a HEADLINE, this entry's
    // own line or a child's, is REFUSED and says so, deleting an entry being
    // neither what this sheet is for nor something a command backs.  The drawer
    // and the planning line are the PANEL's, and `pdelete' is their own half of
    // the same gesture.  One write for the set, however many blocks it names.
    function ddelete(ids, how) {
      const gone = new Set(ids);
      const named = drows.filter((r) => gone.has(r.id));
      const taken = named.filter((r) => r.kind === "para");
      if (named.length !== taken.length)
        append("sync", "warn",
               "a headline is not deleted from the sheet — this writes elements only");
      if (!taken.length) { echo(`D → org-delete-element (${how(0)})`); return; }
      const body = bodyText(new Set(taken.map((r) => r.id)));
      commitDocWith(body,
        () => echo(`D → org-delete-element (${how(taken.length)} taken)`));
    }
    // The one write the document makes, and BODY is the caller's because a
    // deletion cannot rebuild it out of the model — the paragraphs it took out
    // are still in it.  SAY is the line the 200 earns, and the sheet re-reads on
    // it, which is what makes the model the server's reading of what was written
    // rather than this page's guess at it.
    function commitDocWith(body, say) {
      if (!editing) return;
      const h = editing;
      sync("syncing");
      post(h.id, h.digest, { body, properties: props(), planning: planning() },
           null, h.child)
        .then(outcome)
        .then((a) => { if (editing === h && landed(h, say)(a)) reload(); })
        .catch((e) => stuck(subtreeSheet, e.message));
    }
    // The property panel is a table-view MOUNT, the sheet's RIGHT pane.  A
    // drawer is a list of RECORDS — a key and a value, one shape per row — so
    // the renderer draws it, where the document beside it is a list of KINDS.
    // That buys no rows to style, no cursor to move and no second answer to what
    // a flagged row looks like.
    //
    // MODEL AND VIEW.  `prows' is the model and the mount a view of it, re-set
    // on every change.  A row HOLDS its COMMITTED text; the open row's fields
    // are the edit in progress, which makes a commit the only thing that can
    // make the sheet dirty.  The cursor, the flags and the scrolling are the
    // renderer's.  It stays MODAL, dired's shape rather than a form's: in NAV
    // nothing is focusable, leaving the letters free to be movement.
    //
    // The planning entries are rows of this same list — three FIXED ones in
    // org's order ahead of the properties.  Fixed means the key is org's: RET
    // opens the value alone, an empty value is the entry absent, and a delete
    // CLEARS where it would drop a property.  A row's ID is stable for the
    // sheet's life, so a flag and a selection survive edits above them.
    //
    // The identity property is in neither pane: the server keeps
    // 'hiddenProperties' out of what it hands over and puts them back verbatim.
    // Nothing rowed is nothing flaggable.
    const PLANNING = CFG.planning;
    const PCOLS = [ { key: "key", header: "Key" },
                    { key: "value", header: "Value" } ];
    /**
     * ONE PANEL ROW: a property or one of the three fixed planning entries.
     * `fixed` is the planning rows' — their key is org's rather than the
     * author's, so it cannot be typed over and a delete CLEARS rather than drops.
     * @typedef {object} PropRow
     * @property {string} id   stable for the sheet's life: `PLN:KEYWORD` or `P<n>`.
     * @property {string} key
     * @property {string} val
     * @property {boolean} fixed
     */
    /** @type {PropRow[]} */
