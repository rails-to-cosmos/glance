// THE CAPTURE'S TAG FIELD AND THE VALUE PALETTE, behind an argument list (AGENTS.hs).
// What it takes from the shell arrives as accessors: a handle cannot carry a `let'.
const Capture = ((deps) => {
    const { CFG, EMPTY, NEW_HINT, active, append, askFailed, badgeColor, cellTags,
            docTitle, el,
            failed, fire, getJSON, keyName, leadTyped, materialize, part,
            said, showDraft, targetOf, targets, walkStep } = deps;
    const { queryNow, colsNow, entryNow } = deps;
    let capping = null;   // the tag field's state while it is up
    const capUp = () => !!capping;
    function shutCapture() {
      capping = null;
      el("klist").textContent = "";
      el("ktag").value = "";
      shutPopup("capture");
      const held = active();
      if (held && held.blur) held.blur();
    }
    /** `+' ASKS THE DESTINATION FIRST, and nothing else: the tag picks the
     * template, the `#+TODO:' cycle and where the blob lands, so it is settled
     * before there is a document to draw.  The field is SEEDED from the standing
     * filter and never settled by it — a suggestion the reader may back out of
     * in one keystroke. */
    function openCapture(b) {
      sole("capture");
      capping = { b, vocab: [], hot: -1, tag: null };
      el("ktag").value = filteredTag();
      el("klist").textContent = "";
      showPopup("capture", "k", "capture",
                `RET opens the capture · ${EMPTY} tag is the inbox · ESC leaves`);
      el("ktag").focus();
      captureShape(null).then((a) => {
        if (!capping) return;
        capping.vocab = a.tags || [];
        drawTagList(el("ktag").value);
      }).catch(failed(b, "capture"));
    }

    // THE APPLIED QUERY'S OWN PREDICATES, or none where there is no query and no
    // renderer to read one.  EVERY inheritance below reads this one parse.
    const filterTerms = () =>
      (queryNow() && typeof TableView.parseQuery === "function"
        ? TableView.parseQuery(queryNow(), colsNow().map((c) => c.key)) : []);
    /** A FACT THE FILTER PINS TO ONE CONCRETE POSITIVE VALUE, or `""'.  A
     * negated predicate, a WIDENING (an alternative, never a facet every shown
     * row carries), an alternation and a meta (`*archive*') each describe a SET
     * of rows rather than a value a capture could wear. */
    const pinned = (t) =>
      !t.negated && !t.added && t.value
      && !t.value.includes("|") && !/^\*.*\*$/.test(t.value);
    const pinnedTo = (key) => filterTerms().filter((t) => t.key === key && pinned(t));
    // EVERY tag the applied query names, in the order it names them.
    const filteredTags = () => pinnedTo("tag").map((t) => t.value);
    // A capture goes under ONE tag, so it takes the first of them.
    const filteredTag = () => filteredTags()[0] || "";
    /** THE ONE ORDINARY POSITIVE VALUE the filter pins KEY to, or `""'.  Named
     * ONCE is the whole rule: two `state:' predicates describe a union, and a
     * capture inherits from a filter only what that filter leaves no choice
     * about. */
    const soleValue = (key) => {
      const hits = pinnedTo(key);
      return hits.length === 1 ? String(hits[0].value) : "";
    };
    // A DAY THE FILTER PINS: a bare ISO or one word the server resolves.  A
    // comparison, a range or an alternation names a SPAN of days, and a planning
    // entry is one day — so those seed nothing.  READ SYNTACTICALLY: the day
    // WORDS are the server's vocabulary and this page holds no copy of them.
    const ONE_DAY = /^(?:\d{4}-\d{2}-\d{2}|[A-Za-z]+)$/;
    /** WHAT THE STANDING FILTER LENDS A CAPTURE, as the read door's own args.
     * TEMPLATE-FIRST IS THE SERVER'S: these are what the filter leaves no choice
     * about, and the composer there fills only the silences the template left.
     * The destination TAG rides apart, being the capture's address rather than
     * one of its facts. */
    function inherited(tag) {
      const args = [];
      const state = soleValue("state");
      if (state) args.push(["state", state]);
      const priority = soleValue("priority");
      // ORG'S OWN SPELLING IS `[#B]' and the wire takes the letter.
      if (priority) args.push(["priority", priority.replace(/^\[#(.)\]$/, "$1")]);
      // EVERY POSITIVE TAG BEYOND THE DESTINATION joins the draft's own.
      const more = filteredTags().filter((t) => t !== tag);
      if (more.length) args.push(["tags", more.join(",")]);
      for (const word of CFG.settable) {
        const day = soleValue(word.toLowerCase());
        if (day && ONE_DAY.test(day)) args.push([word.toLowerCase(), day]);
      }
      return args;
    }
    function drawTagList(typed) {
      if (!capping) return;
      const want = foldTag(typed);
      capping.shown = capping.vocab
        .filter((t) => !want || foldTag(t).indexOf(want) !== -1).slice(0, 8);
      if (capping.hot >= capping.shown.length) capping.hot = -1;
      const box = el("klist");
      box.textContent = "";
      capping.shown.forEach((t, i) =>
        part(box, "div", i === capping.hot ? "ke kh" : "ke", t));
    }
    /** THE TAG SETTLES AND THE DOCUMENT OPENS.  The server expands the tag's
     * template and answers a DRAFT — the shape `/headline' serves, from bytes
     * that exist only in that answer — and the sheet draws it as it draws any
     * doc.  The form is DOWN by then: there is one editor, and this was the
     * question that had to precede it. */
    function settleTag() {
      const picked = capping.hot >= 0 ? capping.shown[capping.hot] : null;
      if (picked) el("ktag").value = picked;
      const tag = foldTag(el("ktag").value);
      capping.tag = tag; capping.hot = -1;
      el("klist").textContent = "";
      const b = capping.b;
      captureShape(tag || null, inherited(tag)).then((a) => {
        if (!capping || capping.tag !== tag) return;
        shutCapture();
        showDraft(b, tag, a);
      }).catch(failed(b, "capture"));
    }
    // Behind the dispatch; a key another surface claimed is left alone.
    document.addEventListener("keydown", (e) => {
      if (!capping || e.defaultPrevented) return;
      const held = active();
      const k = keyName(e);
      if (held === el("ktag")) {
        const walk = walkStep(k);
        if (walk) {
          capping.hot = Math.max(-1, Math.min(capping.hot + walk,
                                              (capping.shown || []).length - 1));
          drawTagList(el("ktag").value); e.preventDefault(); return;
        }
        // THE ONE FIELD, and RET carries it: dry over an offer, final over the
        // line the reader typed — the shipped tag field's own rule.
        if (k === "RET" || k === "TAB") { settleTag(); e.preventDefault(); }
      }
    });
    el("ktag").addEventListener("input", () => {
      if (!capping) return;
      capping.hot = -1; capping.tag = null;
      drawTagList(el("ktag").value);
    });
    const rowsWord = (n) => `${n} row${n === 1 ? "" : "s"}`;
    const foldTag = (t) => String(t || "").trim().toLowerCase();
    const tagFrom = (c) => foldTag(c.tag);
    function overTargets(b, label, k) {
      const ids = targets();
      if (!ids.length) { said(b, "no row"); return; }
      k(b, ids, `${label} · ${rowsWord(ids.length)}`);
    }
    const docTargets = (b, label, k) =>
      k(b, [entryNow().id], `${label} · ${docTitle()}`);
    function askState(b, ids, title) {
      const mine = ask(title,
        (c) => fire(b, "set-state", ids, { keyword: c.keyword },
                    c.keyword === null ? EMPTY : c.keyword),
        "a letter sets it · + adds one · / to search · ESC leaves");
      // WHAT `+' NEEDS: the rows to set it on once the state has been declared.
      mine.states = { b, ids, title };
      statesFor(ids, false).then((answer) => {
        if (prompting === mine) setChoices(answer.sources);
      }).catch(askFailed(mine, "keywords"));
    }
    /** The palette redrawn from the store AS IT IS NOW, a mint having moved it. */
    function restate() {
      const mine = prompting;
      if (!mine || !mine.states) return Promise.resolve(false);
      return statesFor(mine.states.ids, true).then((answer) => {
        if (prompting !== mine) return false;
        setChoices(answer.sources);
        return true;
      }).catch(askFailed(mine, "keywords"));
    }
    /** THE DRAFT THOSE IDS NAME, or `null'.  A capture names no row, so the two
     * doors keyed by row id — the state palette and the tags popup — read the
     * handle instead, and this is the one place the reading is made. */
    const draftIn = (ids) => {
      const h = entryNow();
      return h && h.capture && ids.length === 1 && ids[0] === h.id ? h : null;
    };
    /** THE STATES IDS MAY BE SET TO.  A DRAFT'S CYCLE CAME WITH ITS ANSWER: the
     * tag's own `#+TODO:' rides the very config file its template does, so the
     * door that expanded the draft is the door that classified it, and no row is
     * named for a read no row could answer.  FRESH RE-ASKS that door, which is
     * what a state MINTED from this very palette needs — the mint wrote the
     * layer, and nothing carried on the draft would have seen it. */
    function statesFor(ids, fresh) {
      const h = draftIn(ids);
      if (!h) return keywordSources(ids);
      if (!fresh) return Promise.resolve({ sources: h.capture.cycle });
      return captureShape(h.capture.tag || null).then((a) => {
        h.capture.cycle = a.cycle || h.capture.cycle;
        return { sources: h.capture.cycle };
      });
    }
    function askTags(b, ids, title) {
      tagsFor(ids).then((answer) => {
        if (!(answer.rows || []).length) { said(b, "no such row"); return; }
        showTags(b, title, answer);
      }).catch(failed(b, "tags"));
    }
    /** WHAT `/tags' ANSWERS FOR A DRAFT.  The VOCABULARY is the whole store's and
     * rides the capture answer already; the one ROW is the draft's own cell.
     * Every write the popup makes goes back out through `fire', where the draft
     * is written — so the popup itself knows nothing about any of this.  NO
     * COUNTS: those are rows per tag, and a draft is in nobody's count yet. */
    function tagsFor(ids) {
      const h = draftIn(ids);
      if (!h) return tagsOf(ids);
      return captureShape(h.capture.tag || null).then((a) => ({
        rows: [{ id: h.id, tags: cellTags((h.cells || {}).tags) }],
        vocabulary: a.tags || [], counts: {}, unknown: [],
      }));
    }
    function planRows(b, keyword) {
      overTargets(b, keyword.toLowerCase(), (bind, ids, title) =>
        askText(title, "RET sets it · empty clears it · ESC leaves", (c) => {
          const date = c.text.trim();
          fire(bind, "set-planning", ids, { keyword, date: date || null },
               date || "cleared");
        }));
    }
    let prompting = null;
    // The claimed letter's INDEX per label, -1 for none, over one a-z pool.
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
    // A declaration so a direct `eval' of this glue leaks it to the harness.
    function letterAt(label, at) {
      return at === -1 ? null : label[at].toLowerCase();
    }
    function raise(title, state, cls, foot) {
      prompting = state;
      el("phead").textContent = title;
      el("pinput").value = "";
      el("prompt").className = "on";
      mode(cls, foot);
      return prompting;
    }
    // `raising' is the keydown that opened the palette, still in flight: the
    // palette's listener sits behind the dispatch and declines that press.
    function ask(title, commit, foot, over) {
      sole(over);
      return raise(title, { choices: [], shown: [], at: 0, commit,
                            narrow: false, raising: true }, "", foot);
    }
    // Letters are stamped IN PLACE: `prompting.table''s cells hold these very
    // objects.  A `fixed' entry carries a key of its own and is out of the pool.
    function offer(list) {
      const pool = list.filter((c) => !c.fixed);
      whichKeys(pool.map((c) => c.label)).forEach((cut, i) => {
        pool[i].cut = cut;
        pool[i].key = letterAt(pool[i].label, cut);
      });
      prompting.choices = list;
      prompting.shown = list;
      if (prompting.narrow) narrowTo(el("pinput").value);
      else drawChoices();
    }
    function setChoices(sources) {
      const flat = [];
      const held = (word) => {
        const c = { label: word, keyword: word, color: badgeColor(word) };
        flat.push(c);
        return c;
      };
      prompting.table = (sources || []).map((s) => ({
        source: s.source,
        cells: [s.active || [], s.inactive || []].map((ws) => ws.map(held)),
      }));
      prompting.meta = { label: EMPTY, keyword: null, meta: true,
                         fixed: true, key: "DEL", cut: -1 };
      flat.push(prompting.meta);
      offer(flat);
    }
    /** RAISING declines the press that opened the palette; a prompt raised from
     * another prompt's COMMIT came through a handled press, so it passes false. */
    function askText(title, foot, commit, raising = true) {
      sole();
      raise(title, { commit, text: true, raising }, "narrow", foot);
      el("pinput").focus();
    }
    function fieldMode(foot) {
      prompting.narrow = true;
      el("pinput").value = "";
      if (prompting.wider) offer(prompting.wider);
      mode("narrow", foot);
      el("pinput").focus();
    }
    // `raising' is cleared here: the press that reached this door came through
    // another surface's listener and has been handled already.
    /** The palette in its typing mode over LIST.  VOCABULARY IS SPELLED AT THE
     * CALL rather than read off the list: `"open"' where the reader may commit a
     * word the list has never held -- the typed line then leads the matches as an
     * offer of its own -- and `"closed"' where the answer must come off the list.
     * @param {"open" | "closed"} vocabulary
     */
    function askFrom(title, list, foot, commit, vocabulary) {
      // Raised OVER the popup that asked for it: this is that popup's own field.
      const mine = ask(title, commit, foot, true);
      mine.raising = false;
      mine.wider = list;
      mine.open = vocabulary === "open";
      fieldMode();
      return mine;
    }
    // TOGGLE rather than assign: `#pbox' carries its size tier as a class too,
    // and a wholesale write drops it silently.
    function mode(cls, foot) {
      el("pbox").classList.toggle("narrow", cls === "narrow");
      if (foot !== undefined) el("pfoot").textContent = foot;
      drawChoices();
    }
    // Blurred as well as hidden: a hidden focused field keeps `typing()' true.
    function unask() {
      prompting = null;
      el("prompt").className = "";
      el("pinput").blur();
    }
    function drawChoices() {
      const list = el("plist");
      list.textContent = "";
      // THE TABLE IS ONE GRID and the rows borrow its tracks, so the class says
      // which shape is being drawn — every other mode is a plain list.
      list.className = prompting.table && !prompting.narrow && !prompting.text
        ? "ptable" : "";
      if (prompting.text) return;
      if (prompting.narrow) {
        prompting.shown.forEach((c, i) => entry(list, "pe"
          + (c.meta ? " pm" : "") + (i === prompting.at ? " pat" : ""), c));
        return;
      }
      if (!prompting.choices.length) {
        part(list, "div", "pnone", "resolving…");
        return;
      }
      // A LIST WITH NO TABLE draws as its entries: source | active | inactive is
      // the KEYWORDS' own shape, and every other letter palette is a plain list.
      if (!prompting.table) {
        prompting.choices.forEach((c) => entry(list, "pe", c));
        return;
      }
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
    function entry(into, cls, c) {
      const row = part(into, "div", cls);
      const marked = !prompting.narrow && c.cut >= 0;
      if (!prompting.narrow && c.fixed) part(row, "span", "pk", c.key);
      const word = part(row, "span", "pw");
      if (c.color) word.style.color = c.color;
      if (!marked) word.textContent = c.label;
      else {
        part(word, "span", "", c.label.slice(0, c.cut));
        const hot = part(word, "b", "", c.label[c.cut]);
        if (c.color) hot.style.textDecorationColor = c.color;
        part(word, "span", "", c.label.slice(c.cut + 1));
      }
      if (c.hint) part(row, "span", "pt", c.hint);
    }
    /** The entries TEXT leaves standing, and where point rests among them.  THE
     * TYPED VALUE IS ALWAYS AN OFFER where the vocabulary is open: it is drawn as
     * its own LEADING entry, hinted as itself, so `RET' commits what was typed
     * and a match is one `C-n' away -- AGENTS.hs.  An empty field offers no
     * literal, and a typed value folding to an entry coincides with it, one entry
     * drawn rather than two. */
    function narrowTo(text) {
      const typed = text.trim();
      const want = typed.toLowerCase();
      // Over the LABEL alone: a digit would otherwise narrow to the `2/3' asides.
      const shown = prompting.choices.filter((c) =>
        c.label.toLowerCase().includes(want));
      const literal = prompting.open && leadTyped(typed, shown.map((c) => c.label));
      prompting.shown = literal
        ? [{ label: typed, tag: typed, hint: NEW_HINT }].concat(shown) : shown;
      prompting.at = 0;
      drawChoices();
    }
    function walkChoices(step) {
      const n = prompting.shown.length;
      if (n) prompting.at = Math.max(0, Math.min(n - 1, prompting.at + step));
      drawChoices();
    }
    // Overlay down FIRST, so the commit runs over a page with no prompt on it.
    function takeChoice(chosen) {
      if (!chosen) return;
      const act = prompting.commit;
      unask();
      act(chosen);
    }
    el("pinput").addEventListener("input", (e) =>
      prompting && !prompting.text && narrowTo(targetOf(e).value));
    el("prompt").addEventListener("click", (e) =>
      { if (e.target === el("prompt")) unask(); });
    // ONE parameter per id: a fallback row id is a path, a comma in one would
    // split it, and encoding cannot help — the server splits after decoding.
    const askIds = (route, ids) =>
      getJSON(route + "?"
        + ids.map((i) => "ids=" + encodeURIComponent(i)).join("&"));
    const keywordSources = (ids) => askIds("/keywords", ids);
    const linksOf = (id) => getJSON(`/links?id=${encodeURIComponent(id)}`);
    /** `GET /capture': the expanded DRAFT for TAG, with what the standing filter
     * LENDS it riding as MORE.  A null tag is the inbox and its default
     * template.  THE READ CREATES NO FILE — a capture is committed or it never
     * was. */
    const captureShape = (tag, more) => {
      const args = (tag === null ? [] : [["tag", tag]]).concat(more || []);
      return getJSON("/capture" + (args.length ? "?" : "")
        + args.map(([k, v]) => `${k}=${encodeURIComponent(v)}`).join("&"));
    };
    const tagsOf = (ids) => askIds("/tags", ids);
    // The server's list (`Glance.Query.followableTypes'), spliced like `CODES'.
    const FOLLOWABLE = CFG.followable;
    const MATERIAL = CFG.material;
    const CODES = CFG.codes;
    const followable = (l) => FOLLOWABLE.indexOf(l.type) !== -1;
    const material = (l) => MATERIAL.indexOf(l.type) !== -1;
    const shortly = (t) => {
      const s = String(t || "");
      return s.length > 80 ? s.slice(0, 79) + "…" : s;
    };
    function followLinks(b, id, a, links) {
      if (!links.length) { said(b, "no links"); return; }
      if (links.length === 1) { openLink(b, links[0]); return; }
      showLinks(b, id, a);
    }
    function openLink(b, link) {
      // AN ORG-GLANCE EDGE NAMES A HEADLINE, and opening it is materializing:
      // the target is the id, whatever the scheme, a `?kind=' suffix dropped.
      if (material(link)) {
        const id = String(link.target)
          .replace(/^[a-z0-9+.-]+:/, "").replace(/\?.*$/, "");
        materialize(id);
        said(b, link.desc);
        append("cmd", "info", `materialized ${JSON.stringify(id)}`);
        return;
      }
      if (!followable(link)) {
        said(b, "link type not implemented");
        append("cmd", "warn", `link type not implemented: ${shortly(link.target)}`);
        return;
      }
      window.open(link.target, "_blank", "noopener");
      said(b, link.desc);
      append("cmd", "info", `link ${JSON.stringify(link.target)} opened`);
    }

    // `prompting' is this widget's own, so it leaves as an answer.
    const promptNow = () => prompting;
    return { whichKeys, letterAt, CODES, ask, askFrom, askState, askTags, askText,
             capUp, docTargets, entry,
             fieldMode, filteredTags, foldTag, followLinks, linksOf, offer,
             keywordSources,
             openCapture, openLink, overTargets, planRows, promptNow, raise,
             restate, rowsWord, shortly, shutCapture, tagFrom, takeChoice, unask,
             walkChoices };
})({ CFG, EMPTY, NEW_HINT, active, append, askFailed, badgeColor, cellTags,
     docTitle, el,
     failed, fire, getJSON, keyName, leadTyped, materialize, part,
     said, showDraft, targetOf, targets, walkStep,
     // FORWARD deps go in as thunks: these are declared in later parts, and a
     // wrapped part's exports are destructured `const's -- naming one here
     // would read it before its initialiser has run.
     showLinks: (...a) => showLinks(...a), showPopup: (...a) => showPopup(...a),
     showTags: (...a) => showTags(...a), sole: (...a) => sole(...a),
     queryNow: () => query, colsNow: () => cols, entryNow: () => editing });
const { CODES, ask, askFrom, askState, askTags, askText, capUp, docTargets, entry,
        fieldMode, filteredTags, foldTag, followLinks, linksOf, offer,
        keywordSources,
        openCapture, openLink, overTargets, planRows, promptNow, raise,
        restate, rowsWord, shortly, shutCapture, tagFrom, takeChoice, unask,
        walkChoices } = Capture;
// The suite drives these two as the pure functions they are, through a direct
// `eval' -- where a `var' reaches the caller's scope and a `const' does not.
var whichKeys = Capture.whichKeys, letterAt = Capture.letterAt;
