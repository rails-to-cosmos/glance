    // `open' is the name a shared URL raises; ROWED means the surface owes a
    // row with it.  The value palette declares neither — AGENTS.hs.
    /** @typedef {object} Surface
     * @property {string} name         what `momentary()' answers with.
     * @property {boolean} [momentary] raised over the sheet rather than beside it.
     * @property {() => boolean} up    is it on screen.
     * @property {() => void} [off]    close it; absent means ESC falls through.
     * @property {() => boolean} [edit] is an edit open INSIDE it.
     * @property {() => void} [shut]   close that edit and leave the surface up.
     * @property {() => boolean} [narrow] is a `/' narrow open INSIDE its list.
     * @property {() => void} [wide]   clear that narrow and leave the surface up.
     * @property {(id?: string|null) => void} [open]  raise it from `?page='.
     * @property {boolean} [rowed]     it needs a row, so its URL carries one.
     * @property {() => string} [panel] the panel it is showing, as the fragment.
     */
    /** @type {Surface[]} */
    const SURFACES = [
      // OVER THE PALETTE, WHICH STANDS: `+' asks for a state the store has not
      // got, and ESC hands the palette back.  First in the list, so ESC walks
      // out of this one before the palette under it.
      { name: "mint", momentary: true, up: mintUp, off: () => shutMint(null) },
      { name: "prompt", momentary: true, up: () => !!promptNow(), off: unask },
      // The picker hangs at the caret and takes no tier, but it is momentary
      // like the rest: ESC walks out of it before anything else.
      { name: "refer", momentary: true, up: referUp, off: () => shutRefer(null) },
      { name: "capture", momentary: true, up: capUp, off: shutCapture,
        open: () => openCapture(RESTORED) },
      // The rowed three open over the row `bootPage' has already landed on.
      { name: "links", momentary: true, up: linking, off: shutLinks,
        edit: lediting, shut: cancelLinkEdit, rowed: true,
        narrow: () => narrowed(linkMount()), wide: () => widen(linkMount(), "ESC"),
        open: () => HANDLERS.openLinks(RESTORED) },
      { name: "tags", momentary: true, up: managing, off: shutTags,
        edit: renaming, shut: cancelRename, rowed: true,
        narrow: () => narrowed(tagMount()), wide: () => widen(tagMount(), "ESC"),
        open: () => overTargets(RESTORED, "tags", askTags) },
      { name: "sheet", up: docHolds, edit: sheetOpen, shut: cancelSheetEdit,
        narrow: () => narrowed(pmount), wide: () => widen(pmount, "ESC"),
        rowed: true, open: (id) => materialize(id) },
      { name: "config", up: () => settings, edit: sediting,
        shut: () => shutEdit(SROW), open: () => openSettings(),
        narrow: () => narrowed(smount), wide: () => widen(smount, "ESC"),
        panel: () => (SECTIONS[ctab] || {}).title },
    ];
    const RESTORED = { seq: "?page", command: "restore-view" };
    const surfaceUp = () => SURFACES.find((s) => s.up()) || null;
    function remembered() {
      const p = params(), s = surfaceUp();
      if (!s || !s.open) { p.delete("page"); p.delete("row"); }
      else {
        p.set("page", s.name);
        const id = s.rowed && focusedId();
        if (id) p.set("row", id); else p.delete("row");
      }
      const at = s && s.panel && s.panel();
      history.replaceState(null, "", `?${p.toString()}${at ? `#${at}` : ""}`);
    }
    function bootPage() {
      const want = params().get("page");
      const s = SURFACES.find((x) => x.name === want && x.open);
      if (!s) return;
      const id = params().get("row");
      const at = (location.hash || "").replace(/^#/, "");
      if (at && s.panel) wantPanel = at;
      if (s.rowed) {
        if (!id) { append("boot", "warn", `${want} needs a row: add &row=ID`); return; }
        if (!can(table, "select") || !table.select(id))
          { append("boot", "warn", `${want}: no row ${id} in this view`); return; }
      }
      s.open(id);
    }
    // The list ORDER breaks one tie: `+' over the tags popup leaves both up.
    const momentary = () =>
      (SURFACES.find((s) => s.momentary && s.up()) || {}).name || null;
    function sole(keep) {
      if (keep) return;
      for (const s of SURFACES) if (s.momentary && s.up()) s.off();
    }
    const typing = () => {
      const a = active();
      return SURFACES.some((s) => s.up())
        || (!!a && (a.tagName === "INPUT" || a.tagName === "TEXTAREA"
                     || a.tagName === "SELECT" || a.isContentEditable));
    };
    const live = (b) => b.scope === "any"
      || (b.scope === "modal" && SURFACES.some((s) => !s.momentary && s.up()))
      || (b.scope === "table" && !typing());
    // A live selection makes C-c and C-x copy and cut, so no prefix claims them.
    function selecting() {
      const a = active();
      if (a && typeof a.selectionStart === "number")
        return a.selectionStart !== a.selectionEnd;
      const s = document.getSelection();
      return !!s && !s.isCollapsed;
    }
    // Returns whether the press was spent — only `DEL''s ladder reads that.
    function clearMarking(b, alsoFlags) {
      if (!marking()) {
        if (alsoFlags) said(b, lacks("marks"));
        return false;
      }
      const n = table.markedCount();
      if (!n && !alsoFlags) return false;
      table.clearMarks();
      if (alsoFlags && flagging()) table.clearFlags();
      said(b, alsoFlags ? "all marks and flags cleared" : String(n));
      return true;
    }
    const named = (b, command) => ({ seq: b.seq, command });
    const HANDLERS = {
      nextRow: () => move(1),
      previousRow: () => move(-1),
      nextColumn: (b) => moveCol(b, 1),
      previousColumn: (b) => moveCol(b, -1),
      nextPage: (b) => turnPage(b, 1),
      previousPage: (b) => turnPage(b, -1),
      firstRow: (b) => endStop(b, false),
      lastRow: (b) => endStop(b, true),
      toggleSort: (b) => {
        if (!wants(b, "sort", "sortPromote")) return;
        const at = column(), c = at === null ? null : cols[at];
        if (!c) { said(b, "no column selected — f/l to pick one"); return; }
        const named = c.header || c.key;
        if (!table.sortPromote(c.key)) { said(b, `${named} does not sort`); return; }
        const chain = table.getSort() || [], head = chain[0];
        said(b, head ? `${named} ${head.ascending !== false ? "▲" : "▼"}` + (chain.length > 1 ? ` · ${chain.length} keys` : "") : named);
      },
      materializeRow: () => {
        const id = focusedId();
        if (id) materialize(id);
        else append("cmd", "info", "no row focused — n or p picks one");
      },
      markToggle: (b) => mark(b, true),
      unmarkRow: (b) => mark(b, false),
      unmarkAll: (b) => clearMarking(b, true),
      markAll: (b) => {
        if (!wants(b, "mark-all", "toggleMark", "markAll")) return;
        // AND IT TOGGLES: `markAll' only ADDS, so an unmoved count means all marked.
        const was = table.markedCount();
        table.markAll();
        const now = table.markedCount();
        if (was && now === was) { table.clearMarks(); said(b, `unmarked · ${was}`); return; }
        said(b, `marked · ${now}`);
      },
      archiveFlag: (b) => flagKey("d", XFLAGS(b), (what) => said(b, what)),
      priorityUp: (b) => cyclePriority(b, 1),
      priorityDown: (b) => cyclePriority(b, -1),
      refer: (b) => referKey(b),
      applyDefault, pinView, relations, focusFilter, toggleRaw, openSettings,
      save: saveSheet,
      commitEdit: (b) => { if (docOpen()) commitDocEdit(b);
                           else if (editing && !raw && !pnav()
                                    && checkboxHere() !== null)
                             toggleCheckbox(b);
                           else said(b, "nothing open here"); },
      archiveRows: (b) => flagKey("D", XFLAGS(b), (what) => said(b, what)),
      flaggedDelete: (b) => flagKey("x", XFLAGS(b), (what) => said(b, what)),
      setState: (b) => overTargets(b, "set state", askState),
      manageTags: (b) => overTargets(b, "tags", askTags),
      capture: (b) => openCapture(b),
      openLinks: (b) => {
        const id = focusedId();
        if (!id) { said(b, "no row"); return; }
        linksOf(id).then((a) => followLinks(b, id, a, a.links || []))
          .catch(failed(b, "open"));
      },
      applyAgenda: (b) => applyView(b, savedQuery("agenda"), (total) => landedAgenda(b, total)),
      schedulePlan: (b) => planRows(b, "SCHEDULED"),
      deadlinePlan: (b) => planRows(b, "DEADLINE"),
      quitWindow: () => {
        if (editing) { leaveSheet(); return; }
        const host = window.webkit && window.webkit.messageHandlers
                       && window.webkit.messageHandlers.quit;
        if (host) { host.postMessage("quit"); return; }
        append("cmd", "info", "q quits the native window; a browser tab closes itself");
      },
      // THREE RUNGS PER SURFACE, innermost first: the edit, the narrow, the surface.
      cancel: () => {
        for (const s of SURFACES) {
          if (s.edit && s.edit()) { s.shut(); return; }
          if (s.up() && s.narrow && s.narrow()) { s.wide(); return; }
          if (s.off && s.up()) { s.off(); return; }
        }
        if (activeSheet()) leaveSheet();
        else if (typing()) active().blur();
      },
      filterDrop: (b) => {
        if (clearMarking(named(b, "unmark-all"), false)) return;
        if (!wants(b, "filter tokens", "stripLastToken", "getQuery")) return;
        if (!table.stripLastToken()) { said(b, "no filter"); return; }
        const left = table.getQuery().trim();
        if (!left && crumbing() && trail().length) {
          const sel = selsFit() ? crumbSels.pop() : null;
          const back = table.popCrumb();
          delete crumbLabels[query];
          applyView(b, back.query, () => said(b, `back to ${back.label}`), sel);
          return;
        }
        commit(left);
        said(b, left ? `filter: ${JSON.stringify(left)}` : "filter cleared");
      },
    };
    function run(b) {
      echo(`${b.seq} → ${b.command}${b.help ? ` · ${b.help}` : ""}`);
      const handler = b.handler && HANDLERS[b.handler];
      if (handler) handler(b);
      else append("cmd", "info", `${b.seq} (${b.command}) — arrives with daemon commands (M4)`);
    }
    document.addEventListener("keydown", (e) => {
      // Listeners ahead of this one claim keys of their own — the sheet's `DEL'.
      if (e.defaultPrevented) return;
      const k = keyName(e);
      if (!k) return;
      const keys = pendingKeys().concat([k]);
      const here = MAPS.rows.filter(live);
      const opens = (b) => keys.every((key, i) => b.keys[i] === key);
      const hit = here.find((b) => b.keys.length === keys.length && opens(b));
      if (hit) {
        prefix([]);
        e.preventDefault();
        if (!(repeating(e) && MAPS.once.indexOf(hit.command) !== -1)) run(hit);
        return;
      }
      if (here.some((b) => b.keys.length > keys.length && opens(b))) {
        if (!selecting()) { e.preventDefault(); prefix(keys); }
        return;
      }
      if (!pendingKeys().length) return;   // not ours; the browser keeps it
      prefix([]);
      if (MAPS.reserved.indexOf(k) === -1) e.preventDefault();
      echo(`${keys.join(" ")} is undefined`);
    });
    document.addEventListener("keydown", (e) => {
      if (!promptNow()) return;
      // The mint form is OVER this one and its fields take letters.
      if (mintUp()) return;
      // The press that RAISED this lands here next — `t' is a letter in it too.
      if (promptNow().raising) { promptNow().raising = false; return; }
      const k = keyName(e);
      if (!k) return;
      if (promptNow().text) {
        if (k !== "RET") return;
        takeChoice({ text: el("pinput").value });
        e.preventDefault();
        return;
      }
      if (!promptNow().narrow) {
        // `+' is out of the a-z pool `whichKeys' draws from, so no entry claims it.
        if (k === "+" && promptNow().states) {
          e.preventDefault();
          openMint();
          return;
        }
        const hit = promptNow().choices.find((c) => c.key === k);
        if (k === "/")
          fieldMode("RET sets it · C-n/C-p walks · ESC leaves");
        // DEL is the popups' own rung wherever no entry CLAIMS it.
        else if (!hit) {
          if (k !== "DEL") return;
          unask();
          keySaid(k)("keyboard-quit");
        }
        else if (!repeating(e)) takeChoice(hit);
        e.preventDefault();
        return;
      }
      const step = walkStep(k);
      if (step) walkChoices(step);
      else if (k === "RET") takeChoice(promptNow().shown[promptNow().at] || freely());
      else return;
      e.preventDefault();
    });
    // `defaultPrevented': the RET that commits `+''s field must not also rename.
    function popupKeys(name, mount, o) {
      document.addEventListener("keydown", (e) => {
        if (momentary() !== name || e.defaultPrevented) return;
        const k = keyName(e);
        if (!k) return;
        if (narrowTyping(mount())) {
          if (narrowPress(k, mount())) e.preventDefault();
          return;
        }
        if (o.editing()) { if (!o.editKeys(k, e)) return; }
        else {
          const step = rowStep(k);
          if (step) stepIn(mount(), step);
          else if (k === "DEL" || k === "q") {
            // A NARROW IS A RUNG UNDER THE POPUP, so `DEL' clears one before it
            // steps out; `q' is `quit-window' and leaves the narrow standing.
            if (k !== "DEL" || !widen(mount(), k)) {
              const surface = SURFACES.find((s) => s.name === name);
              if (surface && surface.off) surface.off();
              keySaid(k)("keyboard-quit");
            }
          }
          else if (!(narrowPress(k, mount()) || o.keys(k, e))) return;
        }
        e.preventDefault();
      });
    }
    popupKeys("links", linkMount, {
      editing: lediting,
      editKeys: (k) => {
        if (k === "TAB" || k === "S-TAB") hop();
        else if (k === "RET") commitLink(edit.row);
        else return false;   // ESC is the keymap's, and puts the link back
        return true;
      },
      keys: (k) => {
        if (k === "o") {
          const link = pointedLink();
          const b = openedBy();
          shutLinks();
          if (link) openLink(b, link);
        }
        else if (k === "RET") openLinkEdit();
        else return false;
        return true;
      },
    });
    popupKeys("tags", tagMount, {
      editing: renaming,
      editKeys: (k) => {
        if (k !== "RET") return false;   // ESC is the keymap's, and puts the tag back
        renameTag(edit.row, el("tname").value);
        return true;
      },
      keys: (k, e) => {
        if (k === "RET") openRename();
        else if (k === "+") addFlow();
        else if (!flagPress(k, e, TFLAGS)) return false;
        return true;
      },
    });

    function apply(frame) {
      const moved = frame.op === "delete-row" ? frame.id : (frame.row || {}).id;
      // `reload' rebuilds both panes, so never over an open edit or unflushed work.
      if (editing && !raw && !sheetOpen() && !dirty() && !pnav()
          && moved === editing.id)
        reload();
      if (!table) return;
      // Only the server knows whether the changed row still matches.
      if (query) return void (clearTimeout(requeryAt),
        requeryAt = setTimeout(() => fetchRows(settled), 250));
      if (frame.op === "upsert-row") table.upsertRow(frame.row);
      else if (frame.op === "delete-row") table.deleteRow(frame.id);
      else return;
      settled();
    }
    function listen() {
      const scheme = location.protocol === "https:" ? "wss" : "ws";
      // The rows came over HTTP; the socket's own set-rows would resend them.
      socket = new WebSocket(`${scheme}://${location.host}/ws?bootstrap=off`);
      socket.onopen = () => {
        backoff = 1000; wash.want("socket", 0);
      };
      socket.onmessage = (e) => apply(JSON.parse(e.data));
      socket.onclose = (e) => {
        socket = null;
        wash.want("socket", 1);
        if (e && e.reason === "view-changed") remount(); else resync();
      };
    }
    function resync() {
      if (!table) { start(); return; }   // nothing mounted yet: this is a boot
      const asked = query;
      load(asking(asked), etag).then((a) => {
        // A daemon restarted while this page was away sent no `view-changed'.
        if (a.view && !sameColumns(a.view.columns || [])) { remount(); return; }
        if (blind) adopt();
        if (a.view && query === asked) { paint(a); settled(); }
        backoff = 1000;
        listen();
        append("ws", "info", a.view ? "reconnected · rows refreshed" : "reconnected");
      }).catch((e) => {
        if (e.indexing) return indexing(e.indexing);
        if (e.name === "AbortError") { listen(); return; }
        quiet(e); again();
      });
    }
    function again() {
      append("ws", "warn", `disconnected · retrying in ${Math.round(backoff / 1000)}s`);
      setTimeout(resync, backoff);
      backoff = Math.min(backoff * 2, 30000);
    }
    // A BOOT THAT BEGAN BLIND OWES ONE RE-READ: `/' does not wait on the walk,
    // so a store still loading serves a page carrying no config.
    let blind = false;
    function indexing(b) {
      blind = true;
      append("boot", "info", `indexing … ${b.elapsed}s`);
      setTimeout(resync, 1000);
    }
    // The tree's saved views, once there is a store to read them from.  The
    // default is RE-APPLIED only where the reader has not made the query theirs.
    function adopt() {
      blind = false;
      getJSON("/config").then((cfg) => {
        const was = savedQuery("default");
        seedViews(cfg.views);
        const now = savedQuery("default");
        bootedOn = now;
        if (now === was || params().has("q") || query !== was) return;
        append("boot", "info", `the tree's default view: ${JSON.stringify(now)}`);
        applyView(bootBinding, now);
      }).catch(quiet);
    }
    const bootBinding = { seq: "", command: "apply-default-filter" };
    function start(after) {
      const asked = (query = bootQuery());
      if (!params().has("q")) remember(asked);
      // A boot takes the first page it can get; a re-application asks for all.
      const swap = !!table;
      const narrow = asking(asked) + (asked ? "&" : "?");
      viewing(load(swap ? asking(asked) : `${narrow}limit=${PAGE}`)).then((a) => {
        if (blind) adopt();
        mount(a.view);
        if (after) after(a.total); else land(null);
        // AFTER THE ROWS ARE IN HAND, so a rowed surface has a row.  Boot only.
        if (!swap) bootPage();
        listen();
        if (!swap && a.total > (a.view.rows || []).length)
          load(asking(asked))
            .then((b) => { if (table && query === asked) paint(b); arm(a.total); })
            .catch(quiet);
        else arm(a.total);
      }).catch((e) => {
        if (e.indexing) return indexing(e.indexing);
        quiet(e); if (e.name !== "AbortError") again();
      });
    }
    append("boot", "info", "loading …");
    start();
