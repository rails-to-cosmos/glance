    // `open' is what a shared URL raises: `?page=NAME', beside the query and
    // the crumbs.  ROWED says the surface needs a row under it, so its URL
    // carries one.  The value palette has none — it is a keystroke's answer
    // rather than a place, and a link with nothing typed into it would restore
    // an empty question.
    /** @typedef {object} Surface
     * @property {string} name         what `momentary()' answers with.
     * @property {boolean} [momentary] raised over the sheet rather than beside it.
     * @property {() => boolean} up    is it on screen.
     * @property {() => void} [off]    close it; absent means ESC falls through.
     * @property {() => boolean} [edit] is an edit open INSIDE it.
     * @property {() => void} [shut]   close that edit and leave the surface up.
     * @property {(id?: string|null) => void} [open]  raise it from `?page='.
     * @property {boolean} [rowed]     it needs a row, so its URL carries one.
     * @property {() => string} [panel] the panel it is showing, as the fragment.
     */
    /** @type {Surface[]} */
    const SURFACES = [
      { name: "prompt", momentary: true, up: () => !!prompting, off: unask },
      { name: "capture", momentary: true, up: capUp, off: shutCapture,
        open: () => openCapture(RESTORED) },
      // The rowed three open the way their keys do, over the row `bootPage'
      // has already landed on — `targets()' and `focusedId()' answer for it.
      { name: "links", momentary: true, up: linking, off: shutLinks,
        edit: lediting, shut: cancelLinkEdit, rowed: true,
        open: () => HANDLERS.openLinks(RESTORED) },
      { name: "tags", momentary: true, up: managing, off: shutTags,
        edit: renaming, shut: cancelRename, rowed: true,
        open: () => overTargets(RESTORED, "tags", askTags) },
      { name: "sheet", up: docHolds, edit: sheetOpen, shut: cancelSheetEdit,
        rowed: true, open: (id) => materialize(id) },
      { name: "config", up: () => settings, edit: sediting,
        shut: () => shutEdit(SROW), open: () => openSettings(),
        panel: () => (SECTIONS[ctab] || {}).title },
    ];
    // What a restored surface echoes as: no key was pressed, so `said' is given
    // the URL as the thing that asked.
    const RESTORED = { seq: "?page", command: "restore-view" };
    // WHICH surface is on screen, by name, and the row under it where it has
    // one — the two halves of `?page='.  Read off the list, so a surface added
    // there is shareable with nothing else to edit.
    const surfaceUp = () => SURFACES.find((s) => s.up()) || null;
    // The URL says what is on screen: `?page=NAME' beside `q', the row where the
    // surface needs one, and the panel as the FRAGMENT.  Called by every raise
    // and every close, so a reader can send the view they are looking at.
    function remembered() {
      const p = params(), s = surfaceUp();
      if (!s || !s.open) { p.delete("page"); p.delete("row"); }
      else {
        p.set("page", s.name);
        const id = s.rowed && focusedId();
        if (id) p.set("row", id); else p.delete("row");
      }
      // ONE WRITER for the whole address: the panel rides as the FRAGMENT and
      // goes with the surface, so a closed sheet leaves neither behind.
      const at = s && s.panel && s.panel();
      history.replaceState(null, "", `?${p.toString()}${at ? `#${at}` : ""}`);
    }
    // AND BACK: the surface a booted URL names, raised once the rows are in
    // hand — a rowed one lands on its row first, so the popup opens over the
    // entry the sender was looking at.  A row the view no longer holds raises
    // nothing and says so.
    function bootPage() {
      const want = params().get("page");
      const s = SURFACES.find((x) => x.name === want && x.open);
      if (!s) return;
      const id = params().get("row");
      // The fragment names the panel, where the surface has panels.
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
    // KEEP exempts a field a surface raises for ITSELF: `+' over the tags popup.
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
        if (alsoFlags) said(b, "this table-view.js has no marks");
        return false;
      }
      const n = table.markedCount();
      if (!n && !alsoFlags) return false;
      table.clearMarks();
      if (alsoFlags && flagging()) table.clearFlags();
      said(b, alsoFlags ? "all marks and flags cleared" : String(n));
      return true;
    }
    // A binding wearing the NAME of the command it delegates to, for the echo.
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
        if (!sorts()) { said(b, "this table-view.js has no sort"); return; }
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
        if (!marking() || !can(table, "markAll"))
          { said(b, "this table-view.js has no mark-all"); return; }
        table.markAll();
        said(b, `marked · ${table.markedCount()}`);
      },
      archiveFlag: (b) => flagKey("d", XFLAGS(b), (what) => said(b, what)),
      priorityUp: (b) => cyclePriority(b, 1),
      priorityDown: (b) => cyclePriority(b, -1),
      applyDefault, pinView, relations, focusFilter, toggleRaw, openSettings,
      save: saveSheet,
      commitEdit: (b) => { if (docOpen()) commitDocEdit(b);
                           else if (editing && !raw && !pnav()
                                    && checkboxAt(drows[dat]) !== null)
                             toggleCheckbox(b);
                           else said(b, "nothing open here"); },
      archiveRows: (b) => flagKey("D", XFLAGS(b), (what) => said(b, what)),
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
      cancel: () => {
        for (const s of SURFACES) {
          if (s.edit && s.edit()) { s.shut(); return; }
          if (s.off && s.up()) { s.off(); return; }
        }
        if (activeSheet()) leaveSheet();
        else if (typing()) active().blur();
      },
      filterDrop: (b) => {
        if (clearMarking(named(b, "unmark-all"), false)) return;
        if (!strips()) { said(b, "this table-view.js has no filter tokens"); return; }
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
      if (!prompting) return;
      // The press that RAISED this lands here next — `t' is a letter in it too.
      if (prompting.raising) { prompting.raising = false; return; }
      const k = keyName(e);
      if (!k) return;
      if (prompting.text) {
        if (k !== "RET") return;
        takeChoice(freely() || { text: el("pinput").value });
        e.preventDefault();
        return;
      }
      if (!prompting.narrow) {
        const hit = prompting.choices.find((c) => c.key === k);
        if (k === "/")
          fieldMode("RET sets it · C-n/C-p walks · ESC leaves");
        // DEL is the popups' own rung — out of a surface with no inner ladder —
        // wherever no entry CLAIMS it; the state palette's `*empty*' claims it
        // and keeps its landed meaning.
        else if (!hit) {
          if (k !== "DEL") return;
          unask();
          keySaid(k)("keyboard-quit");
        }
        else if (!repeating(e)) takeChoice(hit);
        e.preventDefault();
        return;
      }
      const step = k === "<down>" || k === "C-n" ? 1
                 : k === "<up>" || k === "C-p" ? -1 : 0;
      if (step) walkChoices(step);
      else if (k === "RET") takeChoice(prompting.shown[prompting.at] || freely());
      else return;
      e.preventDefault();
    });
    // `defaultPrevented': the RET that commits `+''s field must not also rename.
    function popupKeys(name, mount, o) {
      document.addEventListener("keydown", (e) => {
        if (momentary() !== name || e.defaultPrevented) return;
        const k = keyName(e);
        if (!k) return;
        if (o.editing()) { if (!o.editKeys(k, e)) return; }
        else {
          const step = rowStep(k);
          if (step) stepIn(mount(), step);
          else if (k === "DEL" || k === "q") {
            const surface = SURFACES.find((s) => s.name === name);
            if (surface && surface.off) surface.off();
            keySaid(k)("keyboard-quit");
          }
          else if (!o.keys(k, e)) return;
        }
        e.preventDefault();
      });
    }
    popupKeys("links", () => lmount, {
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
          const b = opening;
          shutLinks();
          if (link) openLink(b, link);
        }
        else if (k === "RET") openLinkEdit();
        else return false;
        return true;
      },
    });
    popupKeys("tags", () => tmount, {
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
      // `reload' rebuilds both panes, so it must not run over an open edit,
      // unflushed work, or the panel's own cursor.
      if (editing && !raw && !sheetOpen() && !dirty() && !pnav()
          && moved === editing.id)
        reload();
      if (!table) return;
      // Only the server knows whether the changed row still matches, and this
      // refetch is where a filtered client's archived rows go — hence `settled'.
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
    // Whole-compare: the state column's badge palette rides inside the columns.
    const sameColumns = (next) => JSON.stringify(next) === JSON.stringify(cols);
    function again() {
      append("ws", "warn", `disconnected · retrying in ${Math.round(backoff / 1000)}s`);
      setTimeout(resync, backoff);
      backoff = Math.min(backoff * 2, 30000);
    }
    function indexing(b) {
      append("boot", "info", `indexing … ${b.elapsed}s · the table opens when the walk lands`);
      setTimeout(resync, 1000);
    }
    function start(after) {
      const asked = (query = bootQuery());
      if (!params().has("q")) remember(asked);
      // A boot takes the first page it can get; a re-application asks for the
      // whole answer, so a full table is never replaced by a partial one.
      const swap = !!table;
      const narrow = asking(asked) + (asked ? "&" : "?");
      viewing(load(swap ? asking(asked) : `${narrow}limit=${PAGE}`)).then((a) => {
        mount(a.view);
        if (after) after(a.total); else land(null);
        // AFTER THE ROWS ARE IN HAND, so a rowed surface has a row to land on.
        // A boot only: a re-application is a view the reader asked for here,
        // and raising a popup over it would be answering a question nobody put.
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
