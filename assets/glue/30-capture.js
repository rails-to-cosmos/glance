    function openCapture(b) {
      sole("capture");
      capping = { b, vocab: [], hot: -1, tag: null, inputs: [] };
      // THE VIEW'S OWN TAG IS THE DEFAULT.  Capturing from a table filtered to
      // one tag almost always means another entry of that kind, so the field
      // opens carrying it and the template it configures is the one that
      // expands.  A SUGGESTION rather than a rule: the field is focused and
      // ordinary, so backspacing to the inbox is one key.
      const seed = filteredTag();
      el("ktag").value = seed; el("ktext").value = "";
      el("kfields").textContent = ""; el("klist").textContent = "";
      showPopup("capture", "k", "capture",
                `RET moves on · at the line it captures · ${EMPTY} tag is the inbox · ESC leaves`);
      el("ktag").focus();
      captureShape(null).then((a) => {
        if (!capping) return;
        capping.vocab = a.tags || [];
        drawTagList(el("ktag").value);
      }).catch(failed(b, "capture"));
      // Settled straight away where there is one, so the template's own asks
      // are on screen before the reader types rather than after they leave the
      // field they never edited.
      if (seed) settleTag();
    }

    // The ONE tag the applied query names, or `""'.  The first positive `tag:'
    // predicate naming a single ordinary value: a negation says which kind this
    // is NOT, an alternation names no one kind, and a starred word is a meta
    // (`*empty*', `*archive*') rather than a tag a capture could wear.  Two
    // different tags name no one kind either, so the first wins and the reader
    // sees which in the field.  Feature-detected like every other renderer
    // capability — an asset with no parser seeds nothing.
    function filteredTag() {
      if (!query || typeof TableView.parseQuery !== "function") return "";
      const named = TableView.parseQuery(query, cols.map((c) => c.key))
        .filter((t) => t.key === "tag" && !t.negated
                    && t.value && !t.value.includes("|") && !/^\*.*\*$/.test(t.value));
      return named.length ? named[0].value : "";
    }
    // The completion under the tag field: the vocabulary narrowed by substring
    // over the folded spelling, at most eight shown, `C-n'/`C-p' and the
    // vertical arrows moving a highlight RET takes.  No highlight commits the
    // field as typed — a tag the tree has never held is reachable, exactly as
    // the palette's `freely' rule had it.
    function drawTagList(typed) {
      if (!capping) return;
      const want = foldTag(typed);
      capping.shown = capping.vocab
        .filter((t) => !want || foldTag(t).indexOf(want) !== -1).slice(0, 8);
      if (capping.hot >= capping.shown.length) capping.hot = -1;
      const box = el("klist");
      box.textContent = "";
      capping.shown.forEach((t, i) => {
        const e = document.createElement("div");
        e.className = i === capping.hot ? "ke kh" : "ke";
        e.textContent = t;
        box.appendChild(e);
      });
    }
    // The tag SETTLES on RET or TAB out of its field: the prompts are fetched
    // and grown in place, and the focus moves to the first of them, else to
    // the line.  Editing the tag afterwards clears the grown fields — they
    // describe a template the field no longer names — and the next settle
    // regrows them.
    function settleTag() {
      const picked = capping.hot >= 0 ? capping.shown[capping.hot] : null;
      if (picked) el("ktag").value = picked;
      const tag = foldTag(el("ktag").value);
      capping.tag = tag; capping.hot = -1;
      el("kfields").textContent = ""; capping.inputs = [];
      el("klist").textContent = "";
      if (!tag) { el("ktext").focus(); return; }
      captureShape(tag).then((a) => {
        if (!capping || capping.tag !== tag) return;
        for (const want of (a.prompts || [])) {
          const row = document.createElement("div");
          row.className = "krow";
          const lab = document.createElement("label");
          lab.className = "klab"; lab.textContent = want;
          const inp = document.createElement("input");
          inp.spellcheck = false;
          row.appendChild(lab); row.appendChild(inp);
          el("kfields").appendChild(row);
          capping.inputs.push({ want, inp });
        }
        (capping.inputs.length ? capping.inputs[0].inp : el("ktext")).focus();
      }).catch(failed(capping.b, "capture"));
    }
    // The form's keys, a document listener like the popups': RET and TAB move
    // the focus forward — tag, each grown field, the line — and RET at the
    // line is the capture.  `C-n'/`C-p' and the vertical arrows walk the tag
    // list while that field holds the focus.  ESC stays the keymap's `cancel',
    // which reaches this surface through `SURFACES' like every other.
    document.addEventListener("keydown", (e) => {
      if (!capping || e.defaultPrevented) return;
      const held = active();
      const k = keyName(e);
      if (held === el("ktag")) {
        const walk = k === "C-n" || k === "<down>" ? 1
                   : k === "C-p" || k === "<up>" ? -1 : 0;
        if (walk) {
          capping.hot = Math.max(-1, Math.min(capping.hot + walk,
                                              (capping.shown || []).length - 1));
          drawTagList(el("ktag").value); e.preventDefault(); return;
        }
        if (k === "RET" || k === "TAB") { settleTag(); e.preventDefault(); }
        return;
      }
      const at = capping.inputs.findIndex((f) => f.inp === held);
      if (at !== -1 && (k === "RET" || k === "TAB")) {
        const next = capping.inputs[at + 1];
        (next ? next.inp : el("ktext")).focus();
        e.preventDefault(); return;
      }
      if (held === el("ktext") && k === "RET") {
        const fields = {};
        for (const f of capping.inputs) fields[f.want] = f.inp.value;
        captureRow(capping.b, el("ktext").value, capping.tag || "", fields);
        e.preventDefault();
      }
    });
    // Typing in the tag field re-narrows the list and orphans any grown
    // fields: they describe a template the field no longer names, and the next
    // settle regrows the right ones.
    el("ktag").addEventListener("input", () => {
      if (!capping) return;
      capping.hot = -1; capping.tag = null;
      el("kfields").textContent = ""; capping.inputs = [];
      drawTagList(el("ktag").value);
    });
    // And the write.  The line is raw org — `TODO Buy milk :errands:' captures a
    // keyword, a title and a tag — and the server decides WHERE: the tree's own
    // `#+GLANCE_CAPTURE_TARGET:' with no tag, a blob in the store with one.  The
    // row comes back over the socket once the watch has read the file, like every
    // write here, and the id the answer carries is what point lands on when it
    // does.
    function captureRow(b, text, tag, fields) {
      const typed = text.trim();
      if (!typed) { said(b, "nothing to capture"); return; }
      const args = { text: typed };
      if (tag) args.tag = tag;
      if (fields && Object.keys(fields).length) args.fields = fields;
      postCommand({ name: "capture", args }).then((a) => {
        arriving = a.id || null;
        // The form closes on the 200 alone: a refusal lands in the log and leaves
        // everything typed where it was, so fixing the line is an edit rather than
        // a retype.
        shutCapture();
        said(b, tag ? `captured · :${tag}:` : `captured · ${a.file}`);
        append("cmd", "info", `headline ${JSON.stringify(typed)} captured into ${a.file}`);
      }).catch(failed(b, "capture"));
    }
    // Reschedule: the same shape as the state palette, over the same rows —
    // marked set, else the row at point — with a line of text where that one has
    // a list.  The server parses the date (ISO, `+3d', `today', or org's own
    // bracketed form) and refuses anything else as the whole request, so an
    // unreadable line moves no row rather than some of them.  An EMPTY line
    // clears the entry, which is how the planning line comes off. A count of
    // rows, pluralised: every surface naming a set of them says it the same way,
    // so the rule sits here rather than at each of them.
    const rowsWord = (n) => `${n} row${n === 1 ? "" : "s"}`;
    // A TAG AS A PALETTE HANDS IT BACK, folded and trimmed.  Presence is folded
    // everywhere on this page, so the three surfaces that take a tag off a
    // palette — the capture chain, the tags popup's add, its rename — read it
    // through one function rather than three copies of the same two calls.
    const foldTag = (t) => String(t || "").trim().toLowerCase();
    const tagFrom = (c) => foldTag(c.tag);
    // The rows a keyed write runs over, and the title that names them: the two
    // keys that ask something before writing count them the same way.
    function overTargets(b, label, k) {
      const ids = targets();
      if (!ids.length) { said(b, "no row"); return; }
      k(b, ids, `${label} · ${rowsWord(ids.length)}`);
    }
    // Its sibling over the DOCUMENT, where the set is settled: a sheet stands on
    // ONE entry, so there is no marked set to inherit and the title names the
    // entry rather than counting rows.  Which is the whole of what the two keys
    // inside the sheet differ from the table's in.
    const docTargets = (b, label, k) =>
      k(b, [editing.id], `${label} · ${docTitle()}`);
    // ASK WHICH STATE, over the rows a key worked out.  The overlay goes up on
    // the press and the answer fills it: the same server that refuses a keyword
    // the row's own file does not declare is the one that says which keywords
    // those are, so the offer and the refusal cannot disagree.  A fill that
    // lands after the reader has left finds another prompt or none, and drops.
    function askState(b, ids, title) {
      const mine = ask(title,
        (c) => fire(b, "set-state", ids, { keyword: c.keyword },
                    c.keyword === null ? EMPTY : c.keyword),
        "a letter sets it · / to search · ESC leaves");
      keywordSources(ids).then((answer) => {
        if (prompting === mine) setChoices(answer.sources);
      }).catch(askFailed(mine, "keywords"));
    }
    // And which tags, which is the POPUP rather than a palette.  Raised LATE,
    // behind the fetch, since no key inside the list it opens is also the key
    // that opens it: an empty mount put up on the press would buy nothing and
    // cost a raising guard.  Every named row unknown to the store leaves nothing
    // to tag at all, which is a refusal rather than an empty popup.
    function askTags(b, ids, title) {
      tagsOf(ids).then((answer) => {
        if (!(answer.rows || []).length) { said(b, "no such row"); return; }
        showTags(b, title, answer);
      }).catch(failed(b, "tags"));
    }
    function planRows(b, keyword) {
      overTargets(b, keyword.toLowerCase(), (bind, ids, title) =>
        askText(title, "RET sets it · empty clears it · ESC leaves", "", (c) => {
          const date = c.text.trim();
          fire(bind, "set-planning", ids, { keyword, date: date || null },
               date || "cleared");
        }));
    }
    // The value palette: a prompt of this page's own, the renderer's overlay
    // belonging to the filter and this page not being allowed to reach into it.
    // ESC is the keymap's own `keyboard-quit', which closes whichever overlay is
    // up.  It opens in WHICH-KEY mode: every entry wears a letter and that letter
    // commits on its own.  The palette IS the confirmation — a reader who pressed
    // `t' has seen the list saying `t' sets TODO — so there is no second key, and
    // the drift lock is what makes a mis-press cheap.  `/' falls back to the
    // completing-read this used to be, for a cycle wide enough that some entry
    // claimed nothing.
    //
    // The keys are handled in a document listener of its own rather than on the
    // field, behind the dispatch.  What holds the SHEET off it is `momentary()',
    // which its listener asks first — `typing()' is the map's guard and reaches
    // the map's rows alone, and the sheet's listener is not one of them.
    let prompting = null;
    // The which-key assignment: each entry claims the first letter of its OWN
    // spelling that no earlier entry took, over one a-z namespace in palette
    // order.  What comes back is that letter's INDEX — the display bolds it
    // there, which is what teaches why DELEGATED is `e' — and -1 for an
    // entry whose every letter was taken.  Pure and order-only, so one tree's
    // cycle always yields the same letters and the muscle memory holds.
    function whichKeys(labels) {
      const taken = new Set();
      return labels.map((label) => {
        for (let i = 0; i < label.length; i += 1) {
          const c = label[i].toLowerCase();
          if (c >= "a" && c <= "z" && !taken.has(c)) { taken.add(c); return i; }
        }
        return -1;
      });
    }
    // A declaration rather than a `const', so a direct `eval' of this glue
    // leaks it the way it leaks `whichKeys': the suite's harness reports the
    // assignment through THIS function rather than re-spelling the rule.
    function letterAt(label, at) {
      return at === -1 ? null : label[at].toLowerCase();
    }
    // The list palette, raised EMPTY on the keydown: `t' and `:' fill it from
    // `/keywords' and `/tags', making the table the resolver's answer rather than
    // this page's guess, and until the fill lands a reader sees the line saying
    // so.
    //
    // `raising' is that keydown, still in flight: this listener sits behind the
    // dispatch, so the press that opened the palette is the next one it sees, and
    // `t' is both the listOpener and a letter in what it opens.  Every palette here
    // is raised that way, so it is set rather than passed.  The prompt itself is
    // handed back, so a fill landing after an ESC can tell that the overlay it
    // was asked for is gone. THE OVERLAY GOING UP, and both doors take it: the
    // STATE the prompt is being raised in, the line its field opens on, the mode
    // the box wears and the foot naming the keys the body cannot draw.  What
    // differs past this is the body — one has entries and the other a line — so
    // it is the state handed in rather than a branch here.
    function raise(title, state, value, cls, foot) {
      prompting = state;
      el("phead").textContent = title;
      el("pinput").value = value;
      el("prompt").className = "on";
      mode(cls, foot);
      return prompting;
    }
    function ask(title, commit, foot, over) {
      sole(over);
      return raise(title, { choices: [], shown: [], at: 0, commit,
                            narrow: false, raising: true }, "", "", foot);
    }
    // LIST under its letters, drawn.  The one place the which-key pool is spent,
    // so the rule a reader learns by heart has one implementation: the state
    // palette hands over its table flattened in draw order, the link palette its
    // own flat list.  The letters are stamped IN PLACE, the table's cells holding
    // these very objects and a copy leaving them holding entries as they were
    // before one was assigned.  An entry that came in with a key of its OWN
    // (`fixed') is out of the pool and out of the assignment: `*empty*' answers
    // to DEL, which is no letter, so the a-z namespace is spent on KEYWORDS alone
    // and the cycle that used to lose one to the meta keeps it.
    function offer(list) {
      const pool = list.filter((c) => !c.fixed);
      whichKeys(pool.map((c) => c.label)).forEach((cut, i) => {
        pool[i].cut = cut;
        pool[i].key = letterAt(pool[i].label, cut);
      });
      prompting.choices = list;
      prompting.shown = list;
      // A reader who pressed `/' and typed while an answer was out is narrowing
      // an empty list; the fill lands in the mode it finds rather than throwing
      // the typing away.
      if (prompting.narrow) narrowTo(el("pinput").value);
      else drawChoices();
    }
    // SOURCES as the palette holds them: the labels down the table's first
    // column, and the flat ordered list everything else reads.  The flattening is
    // the draw order — each source row's active cell and then its inactive one,
    // `*empty*' last — so the letters are assigned ONCE over the whole table and
    // a letter is the reader's wherever in it the keyword sits.  It is also the
    // list `/' narrows, so both modes offer the same entries under the same
    // names.  The letter is folded into each entry HERE, once, and the entry is
    // an OBJECT both halves hold: `table' keeps the cells and `choices' the flat
    // list, the same objects, so the drawing and the dispatch read one field of
    // one thing rather than agreeing on a parallel array's indices — `shown' is
    // narrowed and `choices' is not.  Which is also why the letters are stamped
    // in place: a copy would leave the cells holding the entries as they were
    // before one was assigned.
    function setChoices(sources) {
      const flat = [];
      const held = (word) => {
        const c = { label: word, keyword: word, color: badgeColor(word) };
        flat.push(c);
        return c;
      };
      // Every source is drawn under the name it arrives under: `default',
      // `system', `file' and a tag all read as they are, so this page holds no
      // table of labels to keep in step with the resolver's names.
      prompting.table = (sources || []).map((s) => ({
        source: s.source,
        cells: [s.active || [], s.inactive || []].map((ws) => ws.map(held)),
      }));
      prompting.meta = { label: EMPTY, keyword: null, meta: true,
                         fixed: true, key: "DEL", cut: -1 };
      flat.push(prompting.meta);
      offer(flat);
    }
    // The same overlay with no list in it: one line of text, typed and committed
    // with RET.  It is the minibuffer the filter palette set the pattern for, and
    // it is this one rather than a widget of its own because everything a prompt
    // owes — the band it paints in, the blur on the way out, ESC through the
    // keymap's `cancel' — is already here.  `text' is what the key listener reads
    // to know there is nothing to narrow and nothing a letter would commit; the
    // drawing reads it too and leaves the list empty, so this prompt carries no
    // entries at all.
    function askText(title, foot, initial, commit) {
      sole();
      raise(title, { commit, text: true, raising: true }, initial, "narrow", foot);
      el("pinput").focus();
    }
    // THE FIELD: the completing-read `/' falls back to over the state palette,
    // and the whole of what `+' raises over the tags popup.  What it completes
    // over is `wider', and a line matching nothing commits as written (`freely'),
    // the charset wall that refuses garbage being the server's.  FOOT is the one
    // this mode is to wear, left out where the raise already wrote it: `askFrom'
    // hands `ask' the field's own foot, so a second copy of that string here
    // would be one to keep in step for nothing.
    function fieldMode(foot) {
      prompting.narrow = true;
      el("pinput").value = "";
      if (prompting.wider) offer(prompting.wider);
      mode("narrow", foot);
      el("pinput").focus();
    }
    // The same overlay raised STRAIGHT into that field, with no letters behind
    // it: `+' over the tags popup, whose list is the addable vocabulary and whose
    // commit adds.  One widget for both doors into typing, so what a prompt looks
    // like and how ESC leaves it are decided once.  The raising guard is CLEARED
    // rather than left set: it declines the keydown that OPENED a palette, which
    // is only ever a problem for a key the DISPATCH raised, and the press that
    // reaches here came from another modal surface's own listener and has been
    // handled already, so leaving the guard up would decline the next real key.
    function askFrom(title, list, foot, commit) {
      // Raised OVER the popup that asked for it, which is the one raise that
      // keeps what stood: this is that popup's own field.
      const mine = ask(title, commit, foot, true);
      mine.raising = false;
      mine.wider = list;
      fieldMode();
      return mine;
    }
    // The chrome the mode owns — the box's class, which shows the field, and the
    // foot naming the keys the list cannot draw for itself.  Written at the two
    // transitions, so `drawChoices' stays a list renderer and a keystroke that
    // narrows invalidates nothing outside the list.  An ABSENT foot leaves the
    // one that is there: a transition changing only the mode says so by naming no
    // foot, where passing the string it already wrote would be a second copy.
    // TOGGLE, never assign, for `#sheet''s reason one box over: `#pbox' carries
    // its SIZE TIER as a class too, and a wholesale write dropped it on the first
    // raise — silently, since the markup still reads right and only a live page
    // is a size.  `classList' spells "set one class, keep the rest".
    function mode(cls, foot) {
      el("pbox").classList.toggle("narrow", cls === "narrow");
      if (foot !== undefined) el("pfoot").textContent = foot;
      drawChoices();
    }
    // Blurred as well as hidden: a focused field nobody can see would leave
    // `typing()' true and swallow every key after it.
    function unask() {
      prompting = null;
      el("prompt").className = "";
      el("pinput").blur();
    }
    // The palette is the RESOLUTION, drawn as the layered table it is: one row
    // per source in precedence order, widest first, the source named down the
    // first column and its keywords in the Active and Inactive cells.  What a
    // reader learns from it is why — `TODO' under `default' and `READING' under
    // `book' is the classify chain saying which scope answered.  The hairlines
    // are the rows' own borders and the old active/done split is the two COLUMNS;
    // `*empty*' keeps a spanning row of its own at the foot, in the muted italic
    // every starred meta wears, since no scope declares taking a keyword off.
    // Three shapes, and the mode picks: the text prompt has no list at all, the
    // fallback is the flat list under a cursor, and a table not back yet is the
    // line saying so.
    function drawChoices() {
      const list = el("plist");
      list.textContent = "";
      if (prompting.text) return;
      if (prompting.narrow) {
        prompting.shown.forEach((c, i) => entry(list, "pe"
          + (c.meta ? " pm" : "") + (i === prompting.at ? " pat" : ""), c));
        return;
      }
      // A list that is empty in LETTER mode is a resolution that has not landed:
      // the overlay goes up on the keydown and the answer fills it, and this is
      // the line a reader sees until it does.
      if (!prompting.choices.length) {
        part(list, "div", "pnone", "resolving…");
        return;
      }
      // Past those two the list IS the resolution table: letter mode is the state
      // palette's alone now, and `setChoices' is the only thing that fills one.
      const head = part(list, "div", "pr ph");
      part(head, "div", "ps", "source");
      part(head, "div", "pc", "active");
      part(head, "div", "pc", "inactive");
      prompting.table.forEach((src) => {
        const row = part(list, "div", "pr");
        part(row, "div", "ps", src.source);
        src.cells.forEach((cell) => {
          const box = part(row, "div", "pc");
          cell.forEach((c) => entry(box, "pe", c));
        });
      });
      entry(part(list, "div", "pr pm"), "pe", prompting.meta);
    }
    // One entry: the key token, then the keyword in its badge colour with the
    // claimed letter BOLD where it sits.  The token column goes in the fallback
    // mode, and the bolding with it: no letter commits there, and drawing one
    // would be a lie about what typing it does.
    function entry(into, cls, c) {
      const row = part(into, "div", cls);
      // The letter is marked IN the word, so there is no token beside it — and
      // one exception: a FIXED key names no position in a word (`*empty*' answers
      // to DEL), so that entry alone keeps a token to be told by.  In the
      // fallback mode nothing commits by key at all, so nothing is marked.
      const marked = !prompting.narrow && c.cut >= 0;
      if (!prompting.narrow && c.fixed) part(row, "span", "pk", c.key);
      const word = part(row, "span", "pw");
      if (c.color) word.style.color = c.color;
      if (!marked) word.textContent = c.label;
      else {
        part(word, "span", "", c.label.slice(0, c.cut));
        // The rule under the letter takes the keyword's own hue, which only the
        // entry knows; the weight and the thickness are the stylesheet's.
        const hot = part(word, "b", "", c.label[c.cut]);
        if (c.color) hot.style.textDecorationColor = c.color;
        part(word, "span", "", c.label.slice(c.cut + 1));
      }
      // The muted aside beside an entry: the tag palette's partial count (`2/3'),
      // which is what says a tag only some of the set carries.  A keyword has
      // none, and neither has a levelled tag.
      if (c.hint) part(row, "span", "pt", c.hint);
    }
    function narrowTo(text) {
      const want = text.trim().toLowerCase();
      // Over the LABEL alone.  The aside is drawn rather than searched: the add
      // field writes a coverage count into it and a digit must not narrow the
      // list to the entries that happen to be 2-of-3.
      prompting.shown = prompting.choices.filter((c) =>
        c.label.toLowerCase().includes(want));
      prompting.at = 0;
      drawChoices();
    }
    function walkChoices(step) {
      const n = prompting.shown.length;
      if (n) prompting.at = Math.max(0, Math.min(n - 1, prompting.at + step));
      drawChoices();
    }
    // ONE COMMIT for every mode: a letter, and the field's own RET.  The overlay
    // comes down FIRST, so what the commit sees is a page with no prompt on it —
    // a palette that stayed open over its own write is the shape the tags
    // palette took, and that list is a mount now.
    function takeChoice(chosen) {
      if (!chosen) return;
      const act = prompting.commit;
      unask();
      act(chosen);
    }
    // The typed line as an entry, for a field whose typing REACHES PAST its
    // list — which is what `wider' says: a tag the tree has never held has to be
    // committable, since that is the only way a first one is ever written.
    const freely = () => {
      if (!prompting.wider) return null;
      const typed = el("pinput").value.trim();
      return typed ? { tag: typed } : null;
    };
    // The two fields that hold a LINE rather than a filter narrow nothing: the
    // text prompt has no list, and `+'\''s field is a name being written rather
    // than one being looked for.
    el("pinput").addEventListener("input", (e) =>
      prompting && !prompting.text && narrowTo(targetOf(e).value));
    el("prompt").addEventListener("click", (e) =>
      { if (e.target === el("prompt")) unask(); });
    // What C-c C-t offers: the states the SERVER says those rows may be set to,
    // with the scope that declares each — org's own cycle under `default',
    // `system.org', the row's tags' configs, its file's own `#+TODO:' — plus the
    // entry that takes a keyword off.  Resolved per request, the answer being per
    // ROW: the state column's badges are the union of every file loaded, a
    // superset saying nothing about where a keyword came from, and the column's
    // `values' are the filter's group meta-values (`*active*'), absent from both
    // because no file declares one, so the server refuses every one of them and
    // offering a value that cannot be set is worse than not offering it.
    // `*empty*' wears the stars every reserved meta wears (docs/invariants.md)
    // and is the filter's own word for a cell holding nothing — the entry takes
    // the state column's cell to exactly what `state:*empty*' then finds.  The
    // starred form is the page's mark for a value with semantics rather than a
    // word a file could hold, and the server refuses a starred string as a
    // keyword from the other side.  What it commits is a null keyword, and the
    // key it answers to is DEL — a key that already MEANS take-it-off wherever
    // this page binds one, and no letter, so the a-z pool is spent on KEYWORDS
    // alone and a cycle wide enough to run the pool dry keeps the letter the meta
    // used to take.  In the typing mode DEL is the field's own and `*empty*' is
    // reached the way every other entry is, by narrowing to it.
    const EMPTY = "*empty*";
    // The colour is the badge's own, so a keyword reads in the palette as it
    // reads in the table.  Looked up rather than carried: the resolution names
    // keywords, and the hues are the producer's and ride on the state column
    // where every other reader of them finds them.
    const badgeColor = (keyword) =>
      (((cols.find((c) => c.key === "state") || {}).badges || [])
        .find((b) => b.value === keyword) || {}).color || "";
    // ONE parameter per id rather than the comma list a caller types by hand:
    // the fallback row id is a path and a comma in one would split it, and
    // percent-encoding cannot help — the server splits after decoding.  Both
    // id-taking routes ask it the same way, so the rule is spelled once.
    const askIds = (route, ids) =>
      getJSON(route + "?"
        + ids.map((i) => "ids=" + encodeURIComponent(i)).join("&"));
    const keywordSources = (ids) => askIds("/keywords", ids);
    // Where a row points, out of the server's reading of its subtree.  This
    // page holds no org parser, so the bracket grammar stays where the display
    // rule already lives — one link is `[[TARGET][DESC]]' shown as DESC, and a
    // bare URL is its own description.
    const linksOf = (id) => getJSON(`/links?id=${encodeURIComponent(id)}`);
    // What a capture under TAG will ask for, and the vocabulary and codes that
    // come with every answer.  A null tag is the untagged path's own shape.
    const captureShape = (tag) =>
      getJSON(tag === null ? "/capture" : `/capture?tag=${encodeURIComponent(tag)}`);
    // What the rows a tag command names are tagged with, and what else the tree
    // holds.  Per row rather than as a union, because WHICH rows lack a tag is
    // what decides where an add is sent; the union and its partial counts are
    // worked out here, off that.
    const tagsOf = (ids) => askIds("/tags", ids);
    // What a browser tab can be pointed at, which is http(s) and nothing else.
    // Org writes plenty of other link types and `/links' reports them all —
    // `mailto:', `file:', org's `id:', org-glance's own protocols, a bare
    // `[[Title]]' naming a headline — each naming something a tab is not.
    // Following one needs a handler this page does not have yet, so it says so
    // instead of opening a tab on a string a browser will make nothing of.
    //
    // The TYPE decides, the server's own word for the target
    // (`Glance.Query.linkType'): the scheme, folded.  A regex over the target
    // here would be this page deriving a second time what the answer already
    // carries, leaving the popup's badge column and this test two readings of one
    // string.  The LIST is the server's too (`Glance.Query.followableTypes'),
    // spliced the way `PLANNING' is, and is the same list the badge palette gives
    // the warm hues to, so what reads as followable and what opens cannot come
    // apart.
    const FOLLOWABLE = CFG.followable;
    // THE EXPANSION SUBSET, spliced in like `FOLLOWABLE' rather than fetched:
    // it is a property of the BUILD rather than of the tree, so the binary that
    // expands a template is the binary that says what it expands, and a stale
    // list has no way to exist.  `GET \/capture' still carries it — that is the
    // wire contract, and a client this page is not still reads it there.
    const CODES = CFG.codes;
    const followable = (l) => FOLLOWABLE.indexOf(l.type) !== -1;
    // A target in a log line, kept to a width the strip can show: an org link
    // target runs to a hash and a path, and the line has other words in it.
    const shortly = (t) => {
      const s = String(t || "");
      return s.length > 80 ? s.slice(0, 79) + "…" : s;
    };
    // One tab, and the log keeps what was followed: a link opened is the one
    // thing a key here does that leaves no trace on the page it was pressed on.
    // `noopener' because the opened page must not reach back into this one.
    //
    // The COMMIT is where a link type is judged, which is why this is one
    // function rather than a filter over the rows: the popup lists everything the
    // row points at, that being what teaches a reader what is in the entry, and a
    // single link takes this same door without a popup at all — so `o' on a row
    // holding one `mailto:' warns and opens nothing.
    // THE GESTURE `o' IS, at either grain: none is a refusal, one opens, and
    // several raise the popup.  The table follows the ROW's links and the
    // document the ELEMENT's, so what differs is the set handed in — the answer
    // travels with it, the popup editing under the digest that answer carried.
    function followLinks(b, id, a, links) {
      if (!links.length) { said(b, "no links"); return; }
      if (links.length === 1) { openLink(b, links[0]); return; }
      showLinks(b, id, a);
    }
    function openLink(b, link) {
      if (!followable(link)) {
        said(b, "link type not implemented");
        append("cmd", "warn", `link type not implemented: ${shortly(link.target)}`);
        return;
      }
      window.open(link.target, "_blank", "noopener");
      // The description, and never a fallback to the target: `/links' has already
      // applied the display rule, so a bare URL arrives described by itself and an
      // empty `desc' is a shape the route cannot produce.
      said(b, link.desc);
      append("cmd", "info", `link ${JSON.stringify(link.target)} opened`);
    }
