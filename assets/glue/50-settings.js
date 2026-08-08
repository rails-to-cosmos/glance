    // Settings, in PANELS: the general preferences, the theme, then one box per
    // keyword layer — a layer being one config file and its `#+TODO:' lines
    // VERBATIM.  The line is the contract org itself reads, so it is what is
    // edited: a chip UI here would be this page guessing at a grammar it has no
    // parser for, and the guess would be what gets written.
    //
    // The sheet is the materialize sheet's pattern, down to the words: no
    // buttons, ESC or the backdrop syncs and closes, `C-x C-s' syncs mid-edit,
    // and the header carries one of the same four states.  The REQUEST is its
    // own — `/config' is a pair of routes of its own — and the rows arrive the
    // way every other write's do, the file watch seeing the config change and
    // reseeding the tree.  The theme panel asks nobody, being a `localStorage'
    // preference that applies as it is picked.
    //
    // ONE STRUCTURE for the panels: a name and the elements under it, in order,
    // so a panel joins by adding an entry and the markup it names — the bodies
    // wear `cpart' so the stylesheet needs no entry of its own.  The list wraps
    // markup rather than building it, the bodies being heterogeneous (labelled
    // inputs, selects, boxes the server fills) and a builder for that shape
    // being a template language this page has no use for: `SECTIONS' owns the
    // names and the order, the markup owns what is under them, and they join by
    // id.  A `parts' id the markup does not carry throws here, at boot, which is
    // where a join like that should fail.
    // A PANEL DECLARES ITS OWN ARRIVAL.  `enter' is what the panel needs doing
    // when it comes on screen, so no caller indexes this list by number: two
    // editors write one layer's cycle now, and each is a VIEW that has to be
    // filled from the model on the way in.
    const SECTIONS = [
      { title: "general", parts: ["cgen"] },
      { title: "theme", parts: ["ctheme"],
        enter: () => { if (srows.length) repaintStates(null); } },
      { title: "keywords", parts: ["clayers", "ceff", "cfoot"],
        enter: () => { if (crows[cat]) showLayer(cat); } },
    ];
    // WHICH panel is on screen, by name — the one question every guard outside
    // this block asks, so none of them counts panels.
    const showing = (title) => (SECTIONS[ctab] || {}).title === title;
    // ONE PANEL AT A TIME, named by its tab: the sheet grew past the height a
    // stacked column can hold, and a panel out of the flow takes its fields out
    // of the tab order with it — so native tabbing still walks exactly what is
    // on screen.  The tabs are BUTTONS, which the same tabbing reaches, and the
    // horizontal arrows walk the strip once one holds the focus.
    const csecs = el("csecs"), ctabs = el("ctabs");
    const cpanes = SECTIONS.map((s) => {
      const sec = part(csecs, "div", "csec");
      for (const id of s.parts) sec.appendChild(el(id));
      return sec;
    });
    const ctabels = SECTIONS.map((s, i) => {
      const b = part(ctabs, "button", "ctab", s.title);
      b.addEventListener("click", () => pickTab(i));
      b.addEventListener("keydown", (e) => {
        const k = keyName(e);
        const step = k === "<right>" ? 1 : k === "<left>" ? -1 : 0;
        if (!step) return;
        e.preventDefault();
        const at = (i + step + SECTIONS.length) % SECTIONS.length;
        pickTab(at);
        ctabels[at].focus();
      });
      return b;
    });
    let ctab = -1;
    function showTab(i) {
      ctab = i;
      cpanes.forEach((p, k) => { p.className = k === i ? "csec on" : "csec"; });
      ctabels.forEach((t, k) => { t.className = k === i ? "ctab on" : "ctab"; });
    }
    // PICKING one is the boot's `showTab' plus the model: the box being left is
    // read back before it goes and the panel arriving is filled from the model,
    // so an edit made in either editor of a cycle is what the other opens on.
    // The boot calls `showTab' alone — the sheet's own state is a hundred lines
    // below this and untouchable from here.
    function pickTab(i) {
      takeLayer();
      showTab(i);
      if (SECTIONS[i].enter) SECTIONS[i].enter();
    }
    // TAB IS THE TAB KEY: it walks the panels, wrapping, and `S-TAB' walks back
    // — the sheet is a set of panels and this is the key that says so.  The new
    // panel's FIRST control takes the focus, so a reader lands where they can
    // type; a panel with none leaves the focus on the strip.  Its own listener,
    // registered ahead of the dispatch like the other modal ones and claiming
    // nothing while the sheet is shut or a momentary popup stands over it.
    function stepTab(step) {
      pickTab((ctab + step + SECTIONS.length) % SECTIONS.length);
      const first = cpanes[ctab].querySelector("input, select, textarea");
      if (first) first.focus(); else ctabels[ctab].focus();
    }
    document.addEventListener("keydown", (e) => {
      if (!settings || momentary()) return;
      const k = keyName(e);
      if (k !== "TAB" && k !== "S-TAB") return;
      e.preventDefault();
      stepTab(k === "TAB" ? 1 : -1);
    });
    showTab(0);
    // The layers, and WHICH of them the one box is showing.  `crows' is the
    // whole set with each layer's text in it — the on-screen box is a view of
    // `crows[cat]' rather than the place the text lives, which is what makes a
    // switch cost nothing.
    /** @type {LayerRow[]} */
    let crows = [];
    let settings = false, cat = 0;
    // The settings sheet's half of the pair the ladder drives ('subtreeSheet'
    // is the other): the same four verbs, over the config layers and their own
    // digests, filed under its own log scope.
    const configSheet = {
      noteId: "cnote", scope: "config", state: "synced",
      closed: "settings closed — the files are as they were",
      dirty: () => cdirty(),
      flush: () => flushConfig(),
      refresh: () => config().then((b) => {
        for (const r of crows) {
          const fresh = (b.layers || []).find((l) => l.path === r.path);
          if (fresh) r.digest = fresh.digest;
        }
        return true;
      }),
      shut: () => shutSettings(),
    };
    // Claimed before the fetch, and refused over the other sheet.  `typing()'
    // is not enough to keep the two apart: clicking the materialize sheet's own
    // header blurs its textarea, and a `table' row is live again the moment it
    // does — so the rule is stated here rather than left to the focus.
    function openSettings() {
      if (activeSheet()) return;
      settings = true;
      config().then((b) => {
        if (!settings) return;   // an ESC arrived while the layers were out
        drawLayers(b);
        // The one field on the sheet whose value is this page's own, so it is drawn
        // from storage rather than from the answer — and drawn on every open, which
        // is what puts the preference back over a refused value left in the box.
        el("clog").value = logPref.get();
        cnote("synced");
        el("config").className = "on";
        // The top of the sheet, which is the general panel's first EDITABLE field
        // (the default view above it is read-only, pinned from the table by `P'):
        // the sheet opens where a reader can type, and Tab walks down from there.
        el("ctarget").focus();
      }).catch((e) => {
        settings = false;
        append("config", "error", `settings failed: ${e.message}`);
      });
    }
    const config = () => getJSON("/config");
    function drawLayers(b) {
      crows = (b.layers || []).map(layerRow).sort(byLayer);
      const pick = el("clayer");
      pick.textContent = "";
      crows.forEach((r, i) => {
        const o = part(pick, "option", "", layerName(r));
        o.value = String(i);
      });
      showLayer(0);
      // The capture target is `system.org''s tree-wide LINE, bound to the system
      // layer's row and out in its write: one file, one digest, one splice,
      // wherever on the sheet it is drawn.  The FIRST
      // system layer, which `/config' always serves and always leads with — a tree
      // with none is a server that broke its own contract, and the throw lands in
      // `openSettings''s catch as a settings failure rather than as a sheet that
      // silently drops what a reader types.  The DEFAULT VIEW beside it is
      // READ-ONLY here: composing a query belongs to the table's own widget —
      // badges, completion, the grammar — and `P' pins the applied view as the
      // default, so this field shows what is pinned and never rides a write.
      const cap = el("ctarget");
      cap.value = b.capture || "";
      const sys = crows.find((r) => r.tag === null);
      sys.cap = cap; sys.capBase = cap.value;
      // The composer, mounted ONCE and re-seeded per open: `setQuery' shows the
      // served value without delivering it, and the MAIN table's rows are handed
      // over so value completion offers what the tree actually holds.  An asset
      // without `composer'/`setQuery' still mounts a plain filter bar, which is
      // the graceful floor.  WHICH view it shows is `#cwhich''s, and the ids are
      // the SERVER's — the selector already carries them, so a build with a
      // third view needs nothing here.
      vrows = (b.views || []).map((v) => ({
        id: v.id, base: (v.query || "").trim(), text: (v.query || "").trim() }));
      const which = el("cwhich");
      which.textContent = "";
      vrows.forEach((v) => { part(which, "option", "", `${v.id} view`).value = v.id; });
      vat = vrows.length ? vrows[0].id : "default";
      which.value = vat;
      const shown = vrow() ? vrow().text : "";
      if (!cmpose)
        cmpose = TableView.mount(el("cfbox"), { columns: mainCols, rows: [] },
                                 { composer: true, initialQuery: shown,
                                   onFilter: () => {} });
      else if (can(cmpose, "setQuery")) cmpose.setQuery(shown);
      if (can(cmpose, "setRows") && can(table, "getRows"))
        cmpose.setRows(table.getRows());
      const kw = b.keywords || {};
      el("ceff").textContent =
        `${(kw.active || []).join(" ")} | ${(kw.inactive || []).join(" ")}`;
      drawHues(b, kw);
    }
    // THE TREE'S STATE HUES: which theme is being coloured, and a field per
    // keyword under it.  The MODEL is `hues' — `{theme: {keyword: hue}}` — kept
    // on the system layer beside its other tree-wide lines, so it is `cmoved'
    // and posted the way they are; the fields are a VIEW of `hues[hat]', which
    // is the composer's rule and the layer boxes', so switching themes asks the
    // server nothing and loses no edit.  A keyword with an empty field carries
    // no hue and drops out of the write.
    /**
     * ONE ROW OF THE STATES TABLE: a keyword, the layer that declares it, and
     * which side of that layer's bar it sits on.  `layer` is null for a keyword
     * only a plain org file declares — the tree recognizes it and this sheet
     * cannot move it, which is what `fixed` says.
     * @typedef {object} StateRow
     * @property {string} id
     * @property {LayerRow|null} layer
     * @property {string} state
     * @property {"active"|"inactive"} group
     * @property {boolean} fixed
     */
    /** @type {Record<string, Record<string, string>>} */
    let hues = {};
    /** @type {StateRow[]} */
    let srows = [];
    let huesBase = "", sseq = 0, smount = null;
    // THE STATES TABLE'S COLUMNS.  `tag' names the layer a state BELONGS to,
    // which is the file `+' and `d' write; `colour' is the tree's and rides
    // `system.org' whatever the tag says.
    const SCOLS = [ { key: "tag", header: "Tag" }
                  , { key: "state", header: "State" }
                  , { key: "group", header: "Group" }
                  , { key: "colour", header: "Colour" } ];
    // BY LAYER, THEN CYCLE ORDER: a layer's own `#+TODO:' line left to right,
    // actives before the bar and done-like after, which is the order every
    // other reader of a cycle uses.  `crows' is already system-first then tags
    // alphabetically, so the walk inherits it.  FILE-declared keywords come
    // last, under the tag `file': the tree recognizes them and this sheet
    // cannot move them, so they are listed to be COLOURED and refuse the rest.
    function drawHues(b, kw) {
      hues = {};
      for (const c of b.colors || []) {
        (hues[c.theme] = hues[c.theme] || {})[c.keyword] = c.hue;
      }
      huesBase = JSON.stringify(hues);
      srows = []; sseq = 0;
      const owned = new Set();
      crows.forEach((r) => {
        /** @type {("active"|"inactive")[]} */ (["active", "inactive"]).forEach((group) => {
          (r.kw[group] || []).forEach((state) => {
            owned.add(state);
            srows.push({ id: `S${sseq++}`, layer: r, state, group, fixed: false });
          });
        });
      });
      // The store's palette is wider than the layers: what is left over is a
      // plain file's own `#+TODO:', which no config layer owns.
      (kw.active || []).concat(kw.inactive || []).forEach((state) => {
        if (owned.has(state)) return;
        owned.add(state);
        srows.push({ id: `S${sseq++}`, layer: null,
                     state, group: (kw.inactive || []).includes(state)
                                     ? "inactive" : "active", fixed: true });
      });
      repaintStates(srows.length ? srows[0].id : null);
    }
    // The mount, made once and handed back — the property panel's rule, and for
    // its reason: a mount per open would leave a theme listener behind each time.
    function statesMounted() {
      if (smount) return smount;
      smount = mountOnce("cstates", SCOLS, {
        palette: true, flags: true, actionHints: false,
        flagHelp: "d/D remove · u unflag",
      }, "cstates");
      return smount;
    }
    const stateLabel = (r) => (r.layer ? layerName(r.layer) : "file");
    const srowsOf = () => srows.map((r) => ({
      id: r.id,
      cells: { tag: stateLabel(r), state: r.state, group: r.group,
               colour: (hues[hueTheme()] || {})[r.state] || "" },
    }));
    function repaintStates(at) {
      const m = statesMounted();
      m.setRows(srowsOf());
      if (at) m.select(at);
    }
    // WHICH THEME IS BEING COLOURED is the one on screen, so there is no second
    // selector: `auto' resolves through the media query, the way the boot line
    // does.  Storage stays per theme because readability is — a hue that reads
    // on white is unreadable on black.
    const hueTheme = () => {
      const want = themed.get();
      if (want !== "auto") return want;
      return matchMedia && matchMedia("(prefers-color-scheme:dark)").matches
        ? "dark" : "light";
    };
    // The colour column is the theme on screen's, so a theme pick is a repaint.
    const showHues = () => { if (srows.length) repaintStates(null); };
    // Where the cursor is, in the model's terms — the renderer's answer decides
    // and this page keeps no copy, the property panel's rule.
    const satAt = () => srows.findIndex((r) => r.id === selectedId(smount));
    // THE EDIT OVERLAY, the property panel's mechanism one surface over: three
    // fields, TAB hopping them, RET committing into the MODEL and re-setting the
    // mount, ESC cancelling through `cancel'.  A FILE row's name and group are
    // read-only — no config layer owns them — and its colour is not, since a
    // hue is the tree's.
    const SROW = {
      box: "sedit", pane: "cstates", fields: ["sname", "sgroup", "shue"],
      mount: () => smount,
      fill: (r) => {
        el("sname").value = r.state;
        el("sgroup").value = r.group;
        el("shue").value = (hues[hueTheme()] || {})[r.state] || "";
        el("sname").readOnly = r.fixed;
        el("sgroup").readOnly = r.fixed;
      },
      focus: (r) => (r.fixed || r.state ? el("shue") : el("sname")).focus(),
    };
    const sediting = () => !!edit && edit.o === SROW;
    // A state's name and group are its LAYER's and its colour is the TREE's, so
    // a commit can move two files; both leave in the one flush.
    function commitState() {
      const r = edit.row, was = r.state;
      if (!r.fixed) {
        const name = el("sname").value.trim();
        const group = el("sgroup").value.trim().toLowerCase() === "inactive"
                        ? "inactive" : "active";
        // Out of its old side, into the one named, at the end — a cycle's order
        // is the line's, so a state joins where a reader would type it.
        r.layer.kw.active = r.layer.kw.active.filter((k) => k !== was);
        r.layer.kw.inactive = r.layer.kw.inactive.filter((k) => k !== was);
        r.state = name; r.group = group;
        if (name) r.layer.kw[group].push(name);
        writeCycle(r.layer);
      }
      // The hue follows the NAME: renaming a state carries its colour over,
      // since the pragma is keyed by the word.
      const at = (hues[hueTheme()] = hues[hueTheme()] || {});
      const hue = el("shue").value.trim();
      delete at[was];
      if (hue && r.state) at[r.state] = hue;
      shutEdit(SROW);
      repaintStates(r.id);
    }
    // `+' ADDS A STATE, into the layer the cursor stands in — a new state
    // belongs to a file, and the row under point is the only thing here that
    // names one.  A tree with no config layer at all has nowhere to put it.
    function addState() {
      const at = satAt();
      const host = (at !== -1 && srows[at].layer) || crows.find((r) => !r.tag);
      if (!host) { append("config", "warn", "no config layer to add a state to"); return; }
      /** @type {StateRow} */
      const r = { id: `S${sseq++}`, layer: host, state: "", group: "active",
                  fixed: false };
      srows.splice(at === -1 ? srows.length : at + 1, 0, r);
      repaintStates(r.id);
      openEdit(SROW, r);
    }
    // AND `d'/`D' TAKE A STATE OUT of its layer's line.  A FILE row STANDS: the
    // keyword is a plain org file's own and nothing this sheet writes reaches
    // it, so the take says which and leaves the row where it is.
    function sdelete(ids, how) {
      const gone = new Set(ids);
      const named = srows.filter((r) => gone.has(r.id));
      const stuck = named.filter((r) => r.fixed);
      for (const r of named) {
        if (r.fixed) continue;
        r.layer.kw.active = r.layer.kw.active.filter((k) => k !== r.state);
        r.layer.kw.inactive = r.layer.kw.inactive.filter((k) => k !== r.state);
        writeCycle(r.layer);
      }
      srows = srows.filter((r) => r.fixed || !gone.has(r.id));
      repaintStates(null);
      const held = stuck.map((r) => r.state).join(", ");
      echo(`D → org-todo-remove-state (${how(ids.length - stuck.length)}`
             + `${held ? ` · ${held} is a file's own` : ""})`);
    }
    // The states table's phrases, and its cursor as an ID.
    const SFLAGS = {
      mount: () => smount, take: sdelete, note: unlogged,
      walk: () => stepIn(smount, 1),
      missing: "this table-view.js has no delete flags",
      none: "org-todo-remove-state (no row)",
      unflag: "delete-unflag (flag cleared)",
      flag: "delete-flag (d again removes)",
      at: () => { const i = satAt(); return i === -1 ? null : srows[i].id; },
    };
    // ITS KEYS, a listener of its own behind the sheet's: it claims nothing
    // unless the theme panel is on screen with the table in it, and falls
    // through on every key it does not take.  `RET' opens the row, `+' adds one,
    // `d'/`D'/`u' are dired's gesture through the shape above, and movement is
    // the renderer's own step.
    document.addEventListener("keydown", (e) => {
      if (!settings || momentary() || !smount || !showing("theme")) return;
      const k = keyName(e);
      // The overlay's own keys: TAB hops its three fields, RET commits into the
      // model, and ESC is the keymap's — `cancel' walks `SURFACES' to the rung
      // this shape registered.
      if (sediting()) {
        if (k === "TAB" || k === "S-TAB") { e.preventDefault(); hop(); }
        else if (k === "RET" && !repeating(e)) { e.preventDefault(); commitState(); }
        return;
      }
      if (k === "RET") {
        const at = satAt();
        if (at !== -1) { e.preventDefault(); openEdit(SROW, srows[at]); }
        return;
      }
      if (k === "+") { e.preventDefault(); addState(); return; }
      if (flagPress(k, e, SFLAGS)) { e.preventDefault(); return; }
      const step = k === "n" || k === "j" || k === "<down>" ? 1
                 : k === "p" || k === "k" || k === "<up>" ? -1 : 0;
      if (step) { e.preventDefault(); stepIn(smount, step); }
    });
    // The model as the wire spells it: the flat `{theme, keyword, hue}' list the
    // answer serves, so one shape crosses in both directions.
    const hueList = () =>
      Object.keys(hues).flatMap((theme) =>
        Object.keys(hues[theme]).map((keyword) =>
          ({ theme, keyword, hue: hues[theme][keyword] })));

    // One layer, as this sheet holds it: where it is, what it was read as
    // (`base'), what it says NOW (`text'), the digest a write is pinned to, and
    // whatever the server last said about a write to it.  The text is the row's
    // rather than a box's, which is the whole of what makes switching free.  The
    // two tree-wide fields are the general panel's and are bound to the system
    // layer by `drawLayers'; every layer carries the slots so one shape answers
    // `cmoved' and one shape is posted.
    /**
     * ONE CONFIG LAYER, as this sheet holds it.  The two boxes and the states
     * table are all VIEWS of this record, which is why the shape is stated:
     * a field a server answer stops sending reads as `undefined` at the point
     * of use otherwise, where here it fails at the annotation.
     * @typedef {object} LayerRow
     * @property {string} path       the file, and the write's address.
     * @property {string|null} tag   the tag it configures; null is `system.org`.
     * @property {string} digest     the pin a write to it is checked against.
     * @property {string} base       its `#+TODO:` lines as served.
     * @property {string} text       as this sheet holds them now.
     * @property {string} err        what the server last said about a write.
     * @property {string} tpl        its capture template as served.
     * @property {string} tplBase
     * @property {HTMLInputElement|null} cap  the capture-target field, system only.
     * @property {string|null} capBase
     * @property {{active: string[], inactive: string[]}} kw  the same lines PARSED.
     */
    /**
     * @param {any} layer  one entry of `GET /config`'s `layers`.
     * @returns {LayerRow}
     */
    const layerRow = (layer) => ({
      path: layer.path, tag: layer.tag, digest: layer.digest,
      base: (layer.lines || []).join("\n"),
      text: (layer.lines || []).join("\n"), err: "",
      // The capture template is the layer's SECOND box and is kept the way the
      // first one is: on the row rather than in the box, so switching layers
      // costs no request and loses no edit.
      tpl: layer.template || "", tplBase: layer.template || "",
      cap: null, capBase: null,
      // The same lines PARSED, which the server hands over so this page needs
      // no org grammar to read a cycle.  The states table is a VIEW of this;
      // `writeCycle' renders it back into `text', which is what the flush sends.
      kw: { active: ((layer.keywords || {}).active || []).slice(),
            inactive: ((layer.keywords || {}).inactive || []).slice() },
    });
    // A layer's cycle as its one `#+TODO:' line.  A layer spelling it over two
    // lines comes back spelling it over one, which is what the server's own
    // splice does anyway.  The empty cycle writes NO line, which is how a layer
    // is taken off.
    function writeCycle(r) {
      const act = r.kw.active, done = r.kw.inactive;
      r.text = act.length || done.length
        ? `#+TODO: ${act.join(" ")}${done.length ? ` | ${done.join(" ")}` : ""}`
        : "";
    }
    // SYSTEM FIRST, then the tags in their own alphabet.  The server's order is
    // the walk's, which is where the directories turned up; a reader looking for
    // one tag among forty wants the list they would guess at.  Two system layers
    // keep the order they were served in — `sort' is stable — since nothing
    // distinguishes them but the directory they came from.
    const byLayer = (a, b) => (a.tag === null ? 0 : 1) - (b.tag === null ? 0 : 1)
      || String(a.tag).localeCompare(String(b.tag));
    // A tag layer is named in the grammar's own spelling — the same string
    // a reader would type into the filter box.
    const layerName = (r) => (r.tag ? `tag:${r.tag}` : "system");
    // The box is a VIEW of one layer, so the box's text goes back to the layer
    // it came from before anything else reads or replaces it.  Every door does
    // this first: a switch, a dirty check, a flush.
    // ONLY WHILE THE BOXES ARE ON SCREEN.  The keywords panel's two boxes are a
    // VIEW of the layer, and so is the states table one tab over — both write
    // the same `text'.  A copy-back from a box the reader cannot see would
    // stamp the stale text over whatever the table just wrote, so the box is
    // read only while its own panel is showing and `showLayer' refills it on
    // the way in.
    function takeLayer() {
      if (!crows[cat] || !showing("keywords")) return;
      crows[cat].text = el("ctext").value;
      crows[cat].tpl = el("ctpl").value;
    }
    // What sits AROUND the box, and the only two things a write moves: the label
    // carries the digest, so a layer this sheet just CREATED stops saying it is
    // not there yet, and the line under it carries the layer's last refusal.
    // Drawn on its own because a flush has to redraw both without touching the
    // box the reader may still be typing in.
    function showAround() {
      const r = crows[cat];
      el("clab").textContent = r ? `${layerName(r)} · ${r.path}`
        + (r.digest ? "" : " · not created yet") : "";
      el("clerr").textContent = r ? r.err : "";
    }
    function showLayer(i) {
      cat = Math.max(0, Math.min(i, crows.length - 1));
      el("clayer").value = String(cat);
      el("ctext").value = crows[cat] ? crows[cat].text : "";
      el("ctpl").value = crows[cat] ? crows[cat].tpl : "";
      showAround();
    }
    // Switching layers is a READ, so it writes nothing and asks nobody: the text
    // that was on screen goes back to its layer and the next one's comes out.  An
    // edit outlives every switch, and the sync at the end writes all of them.
    el("clayer").addEventListener("change", (e) => {
      takeLayer();
      showLayer(Number(targetOf(e).value));
    });
    // And switching VIEWS is the same read one row up.
    el("cwhich").addEventListener("change", (e) => showView(targetOf(e).value));
    // `%' IN THE TEMPLATE BOX RAISES THE CODE LIST, which is this page's own
    // value palette in its field mode and no widget of its own.  What it offers
    // is the SERVER's list (`/capture' carries it beside the prompts), so the
    // completion cannot come to offer a code the expansion does not know or
    // omit one it does.  The `%' is not typed — committing writes the whole code
    // and ESC writes nothing — and a literal one is the field's own line, since
    // a line matching no entry commits as written (`freely').
    el("ctpl").addEventListener("keydown", (e) => {
      if (keyName(e) !== "%") return;
      e.preventDefault();
      const box = el("ctpl"), at = box.selectionStart, to = box.selectionEnd;
      askFrom("capture template · which code",
              CODES.map((c) => ({ label: c.code, hint: c.means, tag: c.code })),
              "RET writes it · C-n/C-p walks · ESC leaves",
              (c) => insertCode(at, to, String(c.tag || "")));
    });
    // Back into the box at the caret it was raised from, and the model takes it:
    // the palette blurred the textarea on its way up, so the selection this
    // restores is the one the reader left rather than whatever the browser kept.
    function insertCode(at, to, code) {
      const box = el("ctpl"), text = box.value;
      box.value = text.slice(0, at) + code + text.slice(to);
      box.focus();
      box.setSelectionRange(at + code.length, at + code.length);
      takeLayer();
    }
    // The same four words the other sheet wears, through the same writer.
    const cnote = (next, message) => note(configSheet, next, message);
    const cdirty = () => (takeLayer(), crows.some(cmoved));
    const cmoved = (r) => r.text !== r.base || r.tpl !== r.tplBase
      || (r.tag === null && (movedViews().length > 0 || huesMoved()))
      || (r.cap !== null && r.cap.value !== r.capBase);
    const huesMoved = () => JSON.stringify(hues) !== huesBase;
    // The view showing is the box's, so it is taken back first — every reader
    // of `vrows' calls this, the way every reader of `crows' calls `takeLayer'.
    function takeView() {
      const r = vrow();
      if (r) r.text = composerQuery();
    }
    const movedViews = () => (takeView(), vrows.filter((v) => v.text !== v.base));
    // A view the tree carries NOW: `g' and `a' read these rather than the boot's
    // blob, so a write under a running page is what the next press applies.
    function viewLanded(id, q) {
      if (id === "default") {
        pinnedQuery = q;
        // Compared, never assumed: the reader may have diverged while the write
        // was out, and a badge stamped true then would describe the wrong view.
        if (can(table, "setPinned"))
          table.setPinned(table.getQuery().trim() === pinnedQuery);
      }
      if (id === "agenda") agendaQuery = q;
    }
    // Switching shows the other view's own text and asks the server nothing.
    function showView(id) {
      takeView();
      vat = id;
      if (vrow() && can(cmpose, "setQuery")) cmpose.setQuery(vrow().text);
    }
    // Every layer that moved, one POST each and each awaited — still one
    // drift-locked write per FILE now that the boxes are one box.  A config file
    // is its own write and its own lock, so one that drifted refuses on its own
    // line while the rest land; there is no batch to roll back and none to want.
    // A refusal is the LAYER's, and the sheet goes to the layer that has it: with
    // one box on screen a message under it would otherwise describe a file the
    // reader cannot see.  The FIRST refusal wins the selection, and the log names
    // every one of them, since only one can be shown.
    async function flushConfig() {
      takeLayer();
      cnote("syncing");
      let ok = true, clashed = false, landed = -1;
      for (const r of crows) {
        // A layer this flush has nothing to send for carries no refusal either: its
        // text is the file's again, so a message about the write that was refused
        // describes an edit the reader has since taken back.
        if (!cmoved(r)) { r.err = ""; continue; }
        // What was SENT, taken before the await: a keystroke landing while
        // the write is in flight would otherwise be marked as the file's
        // and never written, and the sheet would close on it silently.
        const sent = r.text, tpl = r.tpl;
        // The views that MOVED, named by id: the server's three-valued rule per
        // view, so one edited leaves the others' lines exactly where they are.
        const views = r.tag === null ? movedViews() : [];
        const colors = r.tag === null && huesMoved() ? hueList() : null;
        const cap = r.cap && r.cap.value;
        const a = await postJSON("/config",
          { path: r.path, lines: sent.split("\n"),
            // The template is named only where it MOVED, which is the absent arm of
            // the server's three-valued rule.  Sending it unconditionally would put
            // every layer's own first heading back through the one-top-entry wall on
            // every write, so a file whose heading is deeper than one — legal org,
            // and no business of this box — could no longer have its cycle edited at
            // all.  The two lines under it have kept this shape all along.
            ...(tpl !== r.tplBase ? { template: tpl } : {}),
            ...(views.length
                  ? { views: Object.fromEntries(views.map((v) => [v.id, v.text])) }
                  : {}),
            ...(colors ? { colors } : {}),
            ...(r.cap ? { capture: cap } : {}),
            digest: r.digest }).then(outcome)
          .catch((e) => ({ status: 0, body: { error: e.message } }));
        if (a.status === 200) {
          r.digest = a.body.digest; r.base = sent; r.tplBase = tpl; r.err = "";
          views.forEach((v) => { v.base = v.text; viewLanded(v.id, v.text); });
          if (colors) huesBase = JSON.stringify(hues);
          if (r.cap) r.capBase = cap;
        } else {
          ok = false;
          if (a.status === 409) clashed = true;
          r.err = a.body.error || `sync failed (${a.status})`;
          if (landed === -1) landed = crows.indexOf(r);
          append("config", "error", `${layerName(r)} · ${r.path}: ${r.err}`);
        }
      }
      // The BOX is left alone where nothing was refused: `C-x C-s' syncs mid-edit,
      // so a reader typing while the write is in flight would have those keystrokes
      // painted over by the text the flush snapshotted.  What is redrawn either way
      // is what sits AROUND it — the label, since a created layer has a digest now,
      // and the refusal line.  A landing takes the box under another `takeLayer',
      // so the in-flight text goes home to its own layer before the swap.
      if (landed === -1) showAround();
      else { takeLayer(); showLayer(landed); }
      cnote(ok ? "synced" : clashed ? "conflict" : "error");
      return ok;
    }
    // C-x C-s and the way out are the ladder's, over `configSheet': the
    // refresh above is what a conflict overwrites under, and the close is
    // pristine-costs-nothing, dirty-syncs-and-closes, trouble-discards.
    function shutSettings() {
      el("config").className = ""; settings = false; crows = []; cat = 0;
      configSheet.state = "synced";
      // And the keys go back to the table, in ONE place.  A control of the sheet
      // holds the focus while it is up — which keeps the table's own keys dead
      // under it — so the close is what has to give it up.  A browser drops the
      // focus anyway when the box goes to `display:none'; saying it makes it the
      // sheet's rule rather than a side effect, covering every control the sheet
      // will ever hold rather than costing one `blur()' per control.
      if (typing()) active().blur();
    }
    // `/' summons the filter.  `openFilter' is the renderer's one entry point
    // for it whatever mode it is in — in palette mode it raises the overlay,
    // elsewhere it takes the box already on the page — so the shell asks for
    // it rather than reaching into the chrome.  An asset predating the call
    // has a resident box; focusing that is how this worked before.
    const summons = () => can(table, "openFilter");
    const focusFilter = () => {
      if (summons()) { table.openFilter(); return; }
      const box = filterBox();
      if (box) { box.focus(); box.select(); }
    };
    // The one exception to keyboard-first, and the reason it is one: a coarse
    // pointer has no `/' to press.  The chip row is the whole of the filter chrome
    // a palette-mode page carries, so it doubles as the palette's button there —
    // the same `focusFilter' the key runs, feature detection included.  Delegated
    // from @#app@, so it survives every re-mount, and gated on the media query the
    // rules are in, so a mouse sees nothing new.  A tap on a chip is that chip's
    // own removal and stays the renderer's.
    const coarse = () => typeof matchMedia === "function"
      && matchMedia("(pointer: coarse)").matches;
    el("app").addEventListener("click", (e) => {
      if (!coarse()) return;
      const t = targetOf(e);
      if (!t.closest || !t.closest(".tv-chips") || t.closest(".tv-chip")) return;
      focusFilter();
    });
    // What a remount takes with it.  The table is `#app''s and goes when
    // the mount is replaced; the palette is the renderer's chrome inside it
    // and goes with it.  The sheet is a SIBLING of `#app' and survives by
    // where it sits, which is a fact about the layout rather than a promise
    // — so both are carried across by hand and neither depends on it.
    let stashed = null;
    // The palette's lifecycle is the renderer's and this page does not reach
    // into its chrome past the field.  A field with focus is a palette the
    // reader is typing in; anything else is a query already committed, which
    // the URL is carrying anyway.
    function typedFilter() {
      const box = filterBox();
      return box && active() === box ? box.value || "" : null;
    }
    function stash() {
      stashed = {
        // A PRISTINE SHEET NEEDS NOTHING: it is a sibling of `#app', so a
        // remount leaves it standing — both panes, the cursor in each, and
        // an open edit with them.  What cannot survive is a sheet that has
        // to be RE-READ, which is a dirty one: the reopen re-materializes
        // and rebuilds both panes, so the reader's work, the element they
        // were standing on and whatever an edit is holding all ride across.
        sheet: editing && dirty()
          ? { id: editing.id, child: editing.child, raw,
              text: el("mtext").value, props: props(), plan: planning(),
              at: drows[dat] ? drows[dat].id : null, col: dcol,
              open: openEditState(), digest: editing.digest }
          : null,
        palette: typedFilter(),
      };
    }
    // The open edit as three strings: which box, which element, and the text its
    // fields are holding.  Null where nothing is open, which is the ordinary
    // sheet — an edit is the one thing on this sheet a commit has not landed.
    function openEditState() {
      if (!docOpen()) return null;
      return { box: dparaing() ? "dpara" : "dtitle", id: edit.row.id,
               val: el(dparaing() ? "dtext" : "dtin").value };
    }
    function restore() {
      const was = stashed;
      stashed = null;
      if (!was) return;
      if (was.palette !== null) {
        focusFilter();
        // Assigning fires no input event, so the renderer is not asked to
        // complete or commit a query the reader has not finished typing.
        const box = filterBox();
        if (box) { box.value = was.palette; box.focus(); }
      }
      if (was.sheet) reopen(was.sheet);
    }
    // The sheet, back open on what was in it — both panes, in the shape it
    // was showing.  The digest is re-asked for rather than carried over: a
    // file that moved while the mount was rebuilt is the conflict flow, and
    // flushing against a digest this page merely remembers is the silent
    // overwrite that flow exists to stop.  The reader's work is put back
    // either way — a restore never decides that an edit is worth less than
    // the file.  The baselines stay the file's, so what was dirty stays dirty.
    function reopen(s) {
      headline(s.id, s.child).then((h) => {
        show(h, s.raw);   // which opens the sheet on the file as it now is
        el("mtext").value = s.text;   // dirty again, against the file now
        if (!s.raw) {
          drawProps(s.props, s.plan);
          const back = drows.findIndex((r) => r.id === s.at);
          if (back !== -1) dat = back;
          dcol = s.col;
          drawDoc();
          if (s.open) reopenEdit(s.open);
        }
        if (h.digest !== s.digest) sync("conflict");
      }).catch((e) => append("sync", "error", `sheet restore failed: ${e.message}`));
    }
    // And the edit that was open, over the element it was open on.  A title cell
    // belongs to the headline element rather than the model, so it is rebuilt
    // from what was stashed; everything else is looked up by id and is gone where
    // the file no longer holds it, a restore declining to invent a row.
    function reopenEdit(o) {
      const r = o.box === "dpara" ? drows.find((x) => x.id === o.id)
                                  : { id: o.id, val: o.val };
      if (!r) return;
      openEdit(o.box === "dpara" ? DPARA : DTITLE, r);
      el(o.box === "dpara" ? "dtext" : "dtin").value = o.val;
    }
    // The one door that throws the mount away and builds a new one: a
    // `view-changed' close, and `g'.  Everything else that loses the socket
    // goes through `resync', which keeps the page it has.
    // An archive's anchor belongs to the VIEW it was taken in, and both doors
    // that replace one drop it: a mount thrown away here, and a new query in
    // `commit'.  Without that, an anchor still armed when a `view-changed'
    // close or `g' rebuilt the table would fire on the next socket frame and
    // pull the cursor off the row the new view had just landed it on.
    function remount(after) { leaving = arriving = null; stash(); start(after); }
    // `g': the view this tree configures, applied the way every other query is —
    // written into the URL and asked of the server.  It goes through the mount
    // because the chips are the renderer's and only a mount can be handed a query
    // it did not commit itself; `start' then reads the URL this just wrote.
    // Dropping onclose first stops the reconnect timer opening a second socket
    // behind this one.  SEL is where the cursor should end up, which only a POP
    // has an opinion about, every other caller leaving it out and taking the
    // first row.  The landing rule lives HERE rather than in each caller, so a
    // view applied through this door lands the same way whoever asked for it.
    function applyView(b, q, landing, sel) {
      said(b, q ? `filter: ${JSON.stringify(q)}` : "filter cleared");
      if (socket) { socket.onclose = null; socket.close(); socket = null; }
      backoff = 1000;
      remember(q);
      remount((total) => { land(sel || null); if (landing) landing(total); });
    }
    // `g' is HOME rather than a step on the trail: it throws the crumbs away
    // with the labels that named them.  Walking back out of a drill is DEL's,
    // one rung at a time, where `g' is the door.
    function applyDefault(b) {
      if (crumbing()) table.setCrumbs([]);
      crumbLabels = {};
      crumbSels = [];
      // The LIVE default, not the boot-baked constant: a pin moves the tree's
      // default under a running page, and `g' a moment later must apply what
      // was just pinned — sort tokens and all — rather than what the page was
      // served with.
      applyView(b, pinnedQuery);
    }
    // THE PIN: the applied query — sort tokens and all, since the order is the
    // grammar's — becomes `system.org''s `#+GLANCE_DEFAULT_FILTER:' line,
    // through the same drift-locked `/config' write the settings sheet rides.
    // Composing stays the table's widget; nothing here parses the query.  The
    // write reseeds and the reseed re-embeds `DEFAULT_QUERY' into the served
    // page, so the pin is the next boot's view; a refusal — a 409 for a config
    // edited elsewhere — is one `cmd' error line and nothing pinned.
    function pinCore(spoke) {
      const q = can(table, "getQuery") ? table.getQuery().trim() : "";
      return getJSON("/config").then((a) => {
        const sys = (a.layers || []).find((l) => !l.tag);
        if (!sys) { spoke("no system layer to pin into"); return; }
        // Through `unwrap', so a refusal THROWS: `postJSON' resolves whatever the
        // status, and a pin that logged success over a 400 is how this shipped
        // reporting \"pinned\" while the file never moved.
        return postJSON("/config",
                        { path: sys.path, digest: sys.digest, views: { default: q } })
          .then(unwrap)
          .then(() => {
            viewLanded("default", q);
            // A sheet standing on the default view is showing the line that just
            // moved, so it is re-seeded; one standing on another view is not.
            const row = vrows.find((v) => v.id === "default");
            if (row) { row.base = q; row.text = q; }
            if (settings && row && row === vrow() && can(cmpose, "setQuery"))
              cmpose.setQuery(q);
            spoke(q ? `pinned · ${q}` : "pinned · all rows");
            append("config", "info",
                   `default view pinned: ${JSON.stringify(q)}`);
          });
      });
    }
    function pinView(b) {
      pinCore((w) => said(b, w)).catch(failed(b, "set-default-view"));
    }
    // The button's voice: no keymap row fired, so the echo spells the command
    // by hand the way the popups' listeners do.
    function pinHere() {
      pinCore((w) => echo(`pin → set-default-view (${w})`))
        .catch((e) => append("config", "error",
                            `set-default-view failed: ${e.message}`));
    }
    // `@': the rows pointing AT the one at point.  A drill is a LOOK, so it
    // takes the row at point and never the marked set — a mark is what a reader
    // lays down to write over a run of rows, and inheriting it here would make
    // every mark change what `@' means.
    //
    // The crumb goes down BEFORE the view changes, so it records where the reader
    // was standing rather than where they landed; `applyView' then writes both
    // into the URL in one `remember'.  `ref:' is the server's own term
    // (SCHEMA.md) — the renderer reads it as free text and would narrow further,
    // which is why the drill re-fetches like every other query.  A drill out of
    // the EMPTY query leaves no crumb, which is the absence of a special case
    // rather than one: `all rows' IS the empty filter and DEL already lands
    // there — the first rung strips the `ref:' token, the query goes empty, and
    // with no trail behind it the key clears the filter, the very view the crumb
    // would have restored — so the crumb, its label and its remembered row would
    // be bookkeeping for a step the ladder takes anyway.  What goes with it is
    // the cursor: DEL back out of that one drill lands on the first row like
    // every other applied view, rather than on the row it was launched from.
    //
    // ZERO REFERENCES IS NO JUMP, and the answer says so: the drill is PROBED
    // first — the same query under `limit=1', a count and one row — and a total
    // of nothing leaves the table, the filter and the trail exactly where they
    // were.  A view with no rows is the one landing a reader cannot read anything
    // off, and walking back out of it costs a keystroke to undo a keystroke; the
    // probe costs a second fetch on a key that was already going to refetch,
    // which is one keypress either way.
    function relations(b) {
      const id = focusedId();
      if (!id) { said(b, "no row"); return; }
      if (!crumbing()) { said(b, "this table-view.js has no crumbs"); return; }
      const token = refToken(id), name = titleOf(id);
      load(`${asking(token)}&limit=1`).then((a) => {
        if (!a.total) {
          said(b, `no references to ${JSON.stringify(name)}`);
          append("cmd", "info", `no references to headline ${JSON.stringify(name)}`);
          return;
        }
        drill(b, token, name);
      }).catch((e) => {
        if (e.name !== "AbortError") failed(b, "relations")(e);
      });
    }

    function drill(b, token, name) {
        if (query.trim()) {
            // The crumb records where the reader was STANDING: the query being left,
            // and the row and column they were on, so walking back puts the cursor
            // where it was rather than at the top of a view they had scrolled into.
            const at = cells() ? table.getSelection() : null;
            const n = table.pushCrumb({ label: hereLabel(), query: query });
            crumbSels[n - 1] = at && at.id ? { id: at.id, col: at.col } : null;
            crumbSels.length = n;
        }
        crumbLabels[token] = `references of «${name}»`;
        applyView(b, token, (total) => said(b, `references of ${JSON.stringify(name)} · ${total}`));
    }
    // `a' is the second canned view and the only one this page spells itself:
    // the active rows carrying a date, which is `planned' — the virtual key over
    // the two date cells, decidable by either side of the wire.  It is a VIEW
    // rather than a mode, so `g' is the way home and every other key means what
    // it always meant while it is applied.  The sort is part of the view —
    // earliest first — so it is part of the QUERY: `sort:scheduled' says it to
    // the server, which answers page one in that order, and to the renderer,
    // which shows it on the header.  A canned view is one string again, where it
    // used to be a query plus a call behind the answer, and `DEL' walks out of
    // the order the way it walks out of the filter.
    // WHICH query is the TREE's, under `#+GLANCE_AGENDA_FILTER:' — the default
    // view's own rule one entry over, editable in the settings sheet and live
    // (`agendaQuery' moves on a write) rather than the constant this page used
    // to spell.  A tree naming no line gets the built-in the server answers with.
    // What `a' says once its rows are on screen.  The count is the server's
    // answer to the query, which is the one number a first page cannot give.
    function landedAgenda(b, total) {
      said(b, `agenda · ${rowsWord(total)}`);
    }

    // Keys.  The map is the JSON above — dispatch and echo read the one blob,
    // and there is one map: `n'/`j' both step a row, `f'/`l' both step a cell.
    const MAPS = JSON.parse(el("keys").textContent);
    // The theme selector.  `auto' is the media query — the attribute comes
    // off — and light and dark pin `data-theme' on the root, which is what
    // this page's own variables and the renderer's overrides both key off.
    // The head has already applied the stored choice; this keeps the
    // control and the storage in step with it.
    // A STORED PREFERENCE, and there are two.  DEF is what a reader gets back
    // when nothing is stored and when storage is denied.  An EMPTIED value is a
    // preference that is not there, so it is REMOVED rather than written as the
    // empty string: what the reader asked for is the default, and a stored `""'
    // would be a preference spelling one.
    const pref = (key, def) => ({
      get() { try { return localStorage.getItem(key) || def; }
              catch (e) { return def; } },
      set(v) {
        try { if (v) localStorage.setItem(key, v);
              else localStorage.removeItem(key); } catch (e) { /* denied */ }
      },
    });
    const themed = pref("glance-theme", "auto");
    function setTheme(name) {
      if (name === "auto") delete document.documentElement.dataset.theme;
      else document.documentElement.dataset.theme = name;
      themed.set(name);
      el("themesel").value = name;
    }
    setTheme(themed.get());
    // The theme applies as it is picked and the sheet stays where it is: this is
    // a preference rather than a write, so there is nothing to sync and no reason
    // to close over it.  The select KEEPS the focus, the whole of the rule now
    // that it lives inside a popup — a popup is a legitimate focus holder,
    // `typing()' is true while one of its controls has the focus, and the table's
    // own keys are dead underneath either way.  A control OUTSIDE a popup owed a
    // hand-written `blur()' for that reason, and no such place is left here.
    el("themesel").addEventListener("change", (e) => {
      setTheme(targetOf(e).value);
      // The hue fields describe the theme on screen, so they follow the pick —
      // the edits already typed stay on the model, each under its own theme.
      if (settings) showHues();
      echo(`theme: ${targetOf(e).value}`);
    });
    // THE LOG'S HEIGHT, the second preference this page keeps for itself.  The
    // stylesheet owns the arithmetic and declares the default; what is written
    // here is the NUMBER, onto the element, so a reader who never opens the
    // sheet and a browser that refuses storage both get the same cap.
    const LOG = CFG.log;
    // What TEXT asks for: a whole number inside the band, the DEFAULT when it
    // names nothing at all, and `null' for everything else — a value this page
    // declines rather than corrects, so a reader typing past the wall keeps the
    // cap they had instead of watching it snap to it.  Half a number on the way
    // to a whole one is the ordinary case of that.
    const logLines = (text) => {
      const t = String(text).trim();
      if (!t) return LOG.def;
      return /^[0-9]+$/.test(t) && +t >= LOG.min && +t <= LOG.max ? +t : null;
    };
    const logPref = pref(LOG.key, "");
    const setLogLines = (n) =>
      el("log").style.setProperty("--g-logn", String(n));
    setLogLines(logLines(logPref.get()) || LOG.def);
    // Applied as it is typed, which makes the field a knob rather than a form:
    // `input' rather than `change', a preference a reader has to leave the field
    // to see being one they cannot aim.  Only a value this page takes is stored,
    // so the box can hold a refused one and the next sheet-open draws the
    // preference back over it.
    el("clog").addEventListener("input", (e) => {
      const n = logLines(targetOf(e).value);
      if (n === null) return;
      logPref.set(String(targetOf(e).value).trim());
      setLogLines(n);
      echo(`log: ${n} lines`);
    });

    // The resident key line, under the log: what can run, where the echo pill says
    // what just did.  The table is the blob's ('Glance.Web.Keymap.keyHints'), naming commands rather
    // than keys, so the spelling comes out of the same rows the dispatch reads.  A
    // command two keys spell shows the FIRST of them, the order 'Glance.Web.Keymap.keyBindings' lists
    // it in.
    function hints() {
      const seq = (command) => {
        const b = MAPS.rows.find((x) => x.command === command && x.scope === "table");
        return b && b.handler ? b.seq : null;   // a staged row is no offer
      };
      el("kbd").textContent = MAPS.hints
        .map((h) => [h.commands.map(seq).filter(Boolean), h.label])
        .filter(([keys]) => keys.length)
        .map(([keys, label]) => `${keys.join("/")} ${label}`)
        .join(" · ");
    }
    hints();
