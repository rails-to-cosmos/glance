// THE VALUE PALETTE AND THE LINK DOOR, behind an argument list (AGENTS.hs).
// What it takes from the shell arrives as accessors: a handle cannot carry a `let'.
const Palette = ((deps) => {
    const { CFG, EMPTY, NEW_HINT, append, askFailed, atIn, badgeColor, docTitle,
            el, entryNow, failed, fire, getJSON, leadTyped, materialize, part,
            said, targetOf, targets } = deps;
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
      keywordSources(ids).then((answer) => {
        if (prompting === mine) setChoices(answer.sources);
      }).catch(askFailed(mine, "keywords"));
    }
    /** The palette redrawn from the store AS IT IS NOW, a mint having moved it. */
    function restate() {
      const mine = prompting;
      if (!mine || !mine.states) return Promise.resolve(false);
      return keywordSources(mine.states.ids).then((answer) => {
        if (prompting !== mine) return false;
        setChoices(answer.sources);
        return true;
      }).catch(askFailed(mine, "keywords"));
    }
    function askTags(b, ids, title) {
      askIds("/tags", ids).then((answer) => {
        if (!(answer.rows || []).length) { said(b, "no such row"); return; }
        showTags(b, title, answer);
      }).catch(failed(b, "tags"));
    }
    /** `set-planning''s ONE ROAD, from every surface that spells a date: the
     * FIELD'S OWN BYTES go out and the server resolves them once against its own
     * clock, an emptied field committing `null' -- `""' is no date and would meet
     * the wall's 400 rather than clear the entry (docs/invariants.md). */
    const firePlanning = (b, ids, keyword, typed) =>
      fire(b, "set-planning", ids, { keyword, date: typed || null },
           typed || "cleared");
    /** THE WALL ABOVE EVERY COMMIT: TEXT trimmed, read against TODAY, and the
     * reader's own word said on B where no reading takes it -- so the server's
     * refusal is the backstop rather than the reader's first news.  Answers the
     * phrase to commit, `""' for the clear, and `null' for what was refused.
     * READ is a surface with a stricter reader than the day's (`readsWhen'). */
    function datePassed(b, text, today, read) {
      const typed = String(text == null ? "" : text).trim();
      if (!typed) return "";
      const r = read ? read(typed) : readsDate(typed, today);
      if (r.ok) return typed;
      said(b, r.why);
      return null;
    }
    function planRows(b, keyword) {
      overTargets(b, keyword.toLowerCase(), (bind, ids, title) =>
        askText(title, DATE_FOOT, (c) => {
          const typed = datePassed(bind, c.text, dateNow());
          if (typed !== null) firePlanning(bind, ids, keyword, typed);
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
    /** The palette in its typing mode over LIST.  VOCABULARY is spelled at the
     * CALL: `"open"' lets the reader commit a word the list never held, leading
     * the matches as an offer of its own; `"closed"' takes only what is listed.
     * `raising' is cleared here -- the press that reached this door came through
     * another surface's listener and has been handled already.
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
      if (n) prompting.at = atIn(prompting.shown, prompting.at + step);
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
    return { whichKeys, letterAt, CODES, ask, askFrom, askState, askTags,
             askText, datePassed, docTargets, entry, fieldMode, firePlanning,
             foldTag, followLinks, keywordSources, linksOf, offer, openLink,
             overTargets, planRows, promptNow, raise, restate, rowsWord,
             shortly, tagFrom, takeChoice, unask, walkChoices };
})({ CFG, EMPTY, NEW_HINT, append, askFailed, atIn, badgeColor, docTitle,
     el, failed, fire, getJSON, leadTyped, materialize, part,
     said, targetOf, targets,
     // A `let' cannot ride in as itself: the open sheet arrives as an accessor.
     entryNow: () => editing });
const { CODES, ask, askFrom, askState, askTags, askText, datePassed, docTargets,
        entry, fieldMode, firePlanning, foldTag, followLinks, keywordSources,
        linksOf, offer, openLink, overTargets, planRows, promptNow, raise,
        restate, rowsWord, shortly, tagFrom, takeChoice, unask,
        walkChoices } = Palette;
// The suite drives these two as the pure functions they are, through a direct
// `eval' -- where a `var' reaches the caller's scope and a `const' does not.
var whichKeys = Palette.whichKeys, letterAt = Palette.letterAt;
