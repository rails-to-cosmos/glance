    // THE FORM'S OWN STATE, which the step-B seam had left in the sheet's file
    // (docs/proposal-widget-files.md): it is up or it is not, and shutting it
    // empties the fields the form itself drew.
    let capping = null;   // the capture form's state while it is up
    const capUp = () => !!capping;
    function shutCapture() {
      capping = null;
      el("kfields").textContent = "";
      el("klist").textContent = "";
      el("ktag").value = ""; el("ktext").value = "";
      el("capture").className = "";
      const held = active();
      if (held && held.blur) held.blur();
    }
    function openCapture(b) {
      sole("capture");
      capping = { b, vocab: [], hot: -1, tag: null, inputs: [] };
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
      if (seed) settleTag();
    }

    // The ONE tag the applied query names: the first positive `tag:' predicate
    // over a single ordinary value.  A starred word is a meta (`*archive*').
    function filteredTag() {
      if (!query || typeof TableView.parseQuery !== "function") return "";
      const named = TableView.parseQuery(query, cols.map((c) => c.key))
        .filter((t) => t.key === "tag" && !t.negated
                    && t.value && !t.value.includes("|") && !/^\*.*\*$/.test(t.value));
      return named.length ? named[0].value : "";
    }
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
    // Behind the dispatch; a key another surface claimed is left alone.
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
    el("ktag").addEventListener("input", () => {
      if (!capping) return;
      capping.hot = -1; capping.tag = null;
      el("kfields").textContent = ""; capping.inputs = [];
      drawTagList(el("ktag").value);
    });
    function captureRow(b, text, tag, fields) {
      const typed = text.trim();
      if (!typed) { said(b, "nothing to capture"); return; }
      const args = { text: typed };
      if (tag) args.tag = tag;
      if (fields && Object.keys(fields).length) args.fields = fields;
      postCommand({ name: "capture", args }).then((a) => {
        arriving = a.id || null;
        shutCapture();
        said(b, tag ? `captured · :${tag}:` : `captured · ${a.file}`);
        append("cmd", "info", `headline ${JSON.stringify(typed)} captured into ${a.file}`);
      }).catch(failed(b, "capture"));
    }
    const rowsWord = (n) => `${n} row${n === 1 ? "" : "s"}`;
    const foldTag = (t) => String(t || "").trim().toLowerCase();
    const tagFrom = (c) => foldTag(c.tag);
    function overTargets(b, label, k) {
      const ids = targets();
      if (!ids.length) { said(b, "no row"); return; }
      k(b, ids, `${label} · ${rowsWord(ids.length)}`);
    }
    const docTargets = (b, label, k) =>
      k(b, [editing.id], `${label} · ${docTitle()}`);
    function askState(b, ids, title) {
      const mine = ask(title,
        (c) => fire(b, "set-state", ids, { keyword: c.keyword },
                    c.keyword === null ? EMPTY : c.keyword),
        "a letter sets it · / to search · ESC leaves");
      keywordSources(ids).then((answer) => {
        if (prompting === mine) setChoices(answer.sources);
      }).catch(askFailed(mine, "keywords"));
    }
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
    function raise(title, state, value, cls, foot) {
      prompting = state;
      el("phead").textContent = title;
      el("pinput").value = value;
      el("prompt").className = "on";
      mode(cls, foot);
      return prompting;
    }
    // `raising' is the keydown that opened the palette, still in flight: the
    // palette's listener sits behind the dispatch and declines that press.
    function ask(title, commit, foot, over) {
      sole(over);
      return raise(title, { choices: [], shown: [], at: 0, commit,
                            narrow: false, raising: true }, "", "", foot);
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
    function askText(title, foot, initial, commit) {
      sole();
      raise(title, { commit, text: true, raising: true }, initial, "narrow", foot);
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
    function askFrom(title, list, foot, commit) {
      // Raised OVER the popup that asked for it: this is that popup's own field.
      const mine = ask(title, commit, foot, true);
      mine.raising = false;
      mine.wider = list;
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
    function narrowTo(text) {
      const want = text.trim().toLowerCase();
      // Over the LABEL alone: a digit would otherwise narrow to the `2/3' asides.
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
    // Overlay down FIRST, so the commit runs over a page with no prompt on it.
    function takeChoice(chosen) {
      if (!chosen) return;
      const act = prompting.commit;
      unask();
      act(chosen);
    }
    // The typed line as an entry: a `wider' field commits past its own list.
    const freely = () => {
      if (!prompting.wider) return null;
      const typed = el("pinput").value.trim();
      return typed ? { tag: typed } : null;
    };
    el("pinput").addEventListener("input", (e) =>
      prompting && !prompting.text && narrowTo(targetOf(e).value));
    el("prompt").addEventListener("click", (e) =>
      { if (e.target === el("prompt")) unask(); });
    // A starred meta (docs/invariants.md): commits a null keyword under DEL.
    // ONE parameter per id: a fallback row id is a path, a comma in one would
    // split it, and encoding cannot help — the server splits after decoding.
    const askIds = (route, ids) =>
      getJSON(route + "?"
        + ids.map((i) => "ids=" + encodeURIComponent(i)).join("&"));
    const keywordSources = (ids) => askIds("/keywords", ids);
    const linksOf = (id) => getJSON(`/links?id=${encodeURIComponent(id)}`);
    const captureShape = (tag) =>
      getJSON(tag === null ? "/capture" : `/capture?tag=${encodeURIComponent(tag)}`);
    const tagsOf = (ids) => askIds("/tags", ids);
    // The server's list (`Glance.Query.followableTypes'), spliced like `CODES'.
    const FOLLOWABLE = CFG.followable;
    const CODES = CFG.codes;
    const followable = (l) => FOLLOWABLE.indexOf(l.type) !== -1;
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
      if (!followable(link)) {
        said(b, "link type not implemented");
        append("cmd", "warn", `link type not implemented: ${shortly(link.target)}`);
        return;
      }
      window.open(link.target, "_blank", "noopener");
      said(b, link.desc);
      append("cmd", "info", `link ${JSON.stringify(link.target)} opened`);
    }
