    const SURFACES = [
      { name: "prompt", momentary: true, up: () => !!prompting, off: unask },
      { name: "capture", momentary: true, up: capUp, off: shutCapture },
      { name: "links", momentary: true, up: linking, off: shutLinks,
        edit: lediting, shut: cancelLinkEdit },
      { name: "tags", momentary: true, up: managing, off: shutTags,
        edit: renaming, shut: cancelRename },
      { name: "sheet", up: docHolds, edit: sheetOpen, shut: cancelSheetEdit },
      { name: "config", up: () => settings, edit: sediting,
        shut: () => shutEdit(SROW) },
    ];
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
      applyAgenda: (b) => applyView(b, agendaQuery, (total) => landedAgenda(b, total)),
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
      const keys = pending.concat([k]);
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
      if (!pending.length) return;   // not ours; the browser keeps it
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
        else if (!hit) return;
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
