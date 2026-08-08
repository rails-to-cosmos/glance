    const SURFACES = [
      { name: "prompt", momentary: true, up: () => !!prompting, off: unask },
      { name: "capture", momentary: true, up: capUp, off: shutCapture },
      { name: "links", momentary: true, up: linking, off: shutLinks,
        edit: lediting, shut: cancelLinkEdit },
      { name: "tags", momentary: true, up: managing, off: shutTags,
        edit: renaming, shut: cancelRename },
      { name: "sheet", up: docHolds, edit: sheetOpen, shut: cancelSheetEdit },
      // The settings sheet, the second WORKSPACE and the fifth surface.  It
      // names no `off': ESC from it falls through to the sheet ladder below,
      // where `activeSheet' already answers for both.  Joining the list is what
      // makes `typing()' see it — an omitted surface leaves every `table' row
      // live underneath, `d' among them, and a click on this sheet's own chrome
      // blurs the field the focus branch was catching it by.
      // Its own rung is the states table's open edit: ESC there restores the row
      // and leaves the sheet standing, exactly as the panel's does one surface up.
      { name: "config", up: () => settings, edit: sediting,
        shut: () => shutEdit(SROW) },
    ];
    // WHICH momentary is up, and there is at most one.  Read off the list, so a
    // fourth is one entry and every reader has it at once.
    const momentary = () =>
      (SURFACES.find((s) => s.momentary && s.up()) || {}).name || null;
    // THE ONE DOOR EXCLUSIVITY IS SPELLED AT.  Every raise passes through here,
    // so "at most one" is a property of the doors rather than a rule the
    // listeners have to keep between them.
    //
    // KEEP is the one exemption, a field a surface raises for ITSELF rather than
    // a stacking pair: `+' over the tags popup raises the palette as THAT POPUP'S
    // OWN FIELD, and the popup is what the typed name goes back into, so it is no
    // second momentary competing with it.  `SURFACES'' order is load-bearing for
    // exactly that pair, through `momentary()', and is stated there. WALKED OFF
    // `SURFACES' rather than restated: a fourth momentary is one entry there and
    // this closes it without an edit, where a hand-written list was a fourth
    // registration site whose omission failed silently.
    function sole(keep) {
      if (keep) return;
      for (const s of SURFACES) if (s.momentary && s.up()) s.off();
    }
    // A focus that keeps its own keys: the filter box, the sheet, and the
    // keys select, which navigates on the arrows this map would otherwise
    // take for row movement — and the modal surfaces, which hold them with
    // nothing focused at all.
    const typing = () => {
      const a = active();
      return SURFACES.some((s) => s.up())
        || (!!a && (a.tagName === "INPUT" || a.tagName === "TEXTAREA"
                     || a.tagName === "SELECT" || a.isContentEditable));
    };
    // `modal' is "a WORKSPACE is up", which is every non-momentary surface:
    // the subtree sheet and the settings sheet.  Never both — `openSettings'
    // refuses over an open sheet, which keeps `C-x C-s' and `ESC' from guessing
    // which one they meant.  Read off `SURFACES' rather than naming the two, so
    // a third workspace is one entry there and this arm has it at once.
    const live = (b) => b.scope === "any"
      || (b.scope === "modal" && SURFACES.some((s) => !s.momentary && s.up()))
      || (b.scope === "table" && !typing());
    // A live selection means C-c and C-x are copy and cut, and the browser
    // decides that on this keydown — so the prefix does not claim them.
    function selecting() {
      const a = active();
      if (a && typeof a.selectionStart === "number")
        return a.selectionStart !== a.selectionEnd;
      const s = document.getSelection();
      return !!s && !s.isCollapsed;
    }
    // ONE IMPLEMENTATION, TWO KEYS.  `U' clears the marks AND the flags; `DEL'
    // clears the MARKS ALONE, since a flag is the archive queue and a backspace
    // must not empty it.  Both speak `unmark-all', the command's own name, so a
    // reader who learns it off either pill can type it back.  The two answers
    // differ over NOTHING TO CLEAR: `U' is the key for this and says so on an
    // asset that has no marks, where `DEL' is a LADDER whose rung that finds
    // nothing has to fall through to the next one silently.  Hence the boolean —
    // "did this key spend its press", which only `DEL' reads.
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
    // A binding wearing another command's NAME, for the one key that delegates:
    // `DEL' really does run `unmark-all', so the pill has to say so, and the
    // echo rule is that the slot after the arrow is the function that ran.
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
      // `^' sorts by the column the CELL selection is standing in, which is the
      // whole of how it picks one: a whole-row selection names no column, and
      // guessing one — the primary, the first, the last one sorted — would be this
      // page inventing a rule the renderer's own `^' does not have (there the
      // answer is where point is), so it refuses and says which key picks a column.
      //
      // `sortable' is the RENDERER's opt-in and `sortBy' ignores it — the flag
      // gates what a reader may reach, where a producer's own call is the
      // producer's business — so a page driving a reader's key has to honour it
      // here or it would sort a column the header click will not.
      //
      // `^' PROMOTES: the column at point becomes the chain's head ascending (the
      // rest shift down, deduped); on the column already leading it flips that key
      // alone.  Composing a chain = pressing over columns in reverse priority
      // order — the web's spelling of table-view.el's C-u ^.
      //
      // IT IS A QUERY EDIT.  The renderer writes the new chain into the applied
      // query as ONE arrow-form `sort:' token and delivers it, so the press lands
      // here as an ordinary filter commit: the rows in hand re-order at once, the
      // URL is rewritten, the server is asked for that order and answers page one
      // in it, and DEL walks the keys back off one at a time, the chain being one
      // chip whose last key the renderer gives up per press.  Nothing on this page
      // remembers a sort.
      toggleSort: (b) => {
        if (!sorts()) { said(b, "this table-view.js has no sort"); return; }
        const at = column(), c = at === null ? null : cols[at];
        if (!c) { said(b, "no column selected — f/l to pick one"); return; }
        const named = c.header || c.key;
        // `sortable' is the renderer's opt-in and `sortPromote' is where it is
        // enforced, so the refusal is READ OFF the call rather than derived a
        // second time here — the key still has to SPEAK it.
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
      // `M' marks the whole loaded set, which is the renderer's call because the
      // set is the renderer's: a page it is not showing is still marked.
      markAll: (b) => {
        if (!marking() || !can(table, "markAll"))
          { said(b, "this table-view.js has no mark-all"); return; }
        table.markAll();
        said(b, `marked · ${table.markedCount()}`);
      },
      // dired's `d' at the table, the gesture `flagKey' holds.  The command is in
      // ONCE, so a HELD `d' delivers exactly one press and can never flag and
      // archive from one keystroke.  `u' takes a flag off, through `mark'.
      archiveFlag: (b) => flagKey("d", XFLAGS(b), (what) => said(b, what)),
      priorityUp: (b) => cyclePriority(b, 1),
      priorityDown: (b) => cyclePriority(b, -1),
      applyDefault, pinView, relations, focusFilter, toggleRaw, openSettings,
      // One `save-buffer' over two sheets: `saveSheet' asks `activeSheet' which
      // is up, so there is nothing to choose between here.
      save: saveSheet,
      // With an element open it commits it; with none, org's own second
      // meaning — the checkbox at point — before the refusal.
      commitEdit: (b) => { if (docOpen()) commitDocEdit(b);
                           else if (editing && !raw && !pnav()
                                    && checkboxAt(drows[dat]) !== null)
                             toggleCheckbox(b);
                           else said(b, "nothing open here"); },
      // D is dired's key and org-glance's `delete', and it is the same gesture
      // with no flagging step in front of it — the same call the second `d' makes,
      // differing in the key it hands over and so in the name the echo spells.
      archiveRows: (b) => flagKey("D", XFLAGS(b), (what) => said(b, what)),
      // C-c C-t asks which state, over whatever the command would run on — the
      // marked set, else the row at point.  The asking is `askState''s, shared with
      // the sheet's own `t'; what this key decides is WHICH ROWS.
      setState: (b) => overTargets(b, "set state", askState),
      // `:' is the agenda's own key for the same question, over the same rows.  It
      // raises the POPUP, which STAYS up under every write it carries: managing
      // tags is several ops over one set where setting a state is one, and closing
      // after each would make the second op a fresh press and a fresh resolution.
      manageTags: (b) => overTargets(b, "tags", askTags),
      // `+' is a CHAIN of prompts and nothing else: which tag, whatever that tag's
      // template asks, then the line.  What it collects goes straight to the
      // server, which knows the file and holds the template.
      capture: (b) => openCapture(b),
      // `o' FOLLOWS the row, and how many links it holds decides the whole gesture:
      // none is a refusal, one opens, several raise the popup.  The count is the
      // server's answer, so the popup can only go up behind the request, which is
      // why this one is raised late where the state palette is raised on the press;
      // by then the `o' that asked has been dispatched and gone, so nothing is
      // travelling and no press is declined.  One consequence, named rather than
      // worked around: the popup is also where a link is EDITED, so a row holding
      // exactly ONE link is followed and never listed, and that link has no editor.
      // Following is what this key promises, and a list of one to pick from would
      // be chrome over every press that meant to open something.  A key that LISTS
      // whatever the count is would settle it.
      openLinks: (b) => {
        const id = focusedId();
        if (!id) { said(b, "no row"); return; }
        linksOf(id).then((a) => followLinks(b, id, a, a.links || []))
          .catch(failed(b, "open"));
      },
      applyAgenda: (b) => applyView(b, agendaQuery, (total) => landedAgenda(b, total)),
      schedulePlan: (b) => planRows(b, "SCHEDULED"),
      deadlinePlan: (b) => planRows(b, "DEADLINE"),
      // `q' is the SUBTREE sheet's door alone, which is why it asks after
      // `editing' rather than after whichever sheet is up.
      // `q' ON THE MAIN PAGE QUITS THE APP, where there is an app to quit: the
      // native window carries a `quit' script-message handler and closing it
      // stops the daemon, the window BEING the app.  A browser tab has no such
      // handler — `window.close()' is refused for a tab a script did not open —
      // so there the key says what it cannot do.  The sheet arm stays for the
      // scope's sake: `q' is a `table' row and a sheet makes `typing()' true, so
      // it is unreachable today and is one line rather than a rule to restate
      // should the scope ever widen.
      quitWindow: () => {
        if (editing) { leaveSheet(); return; }
        const host = window.webkit && window.webkit.messageHandlers
                       && window.webkit.messageHandlers.quit;
        if (host) { host.postMessage("quit"); return; }
        append("cmd", "info", "q quits the native window; a browser tab closes itself");
      },
      // ONE KEY OUT OF WHICHEVER OVERLAY IS UP — the prompt first, being the one
      // that can be raised over an open sheet — walked off `SURFACES' rather than
      // restated as a chain of tests: each surface's OPEN EDIT is the rung under
      // it, so ESC puts a panel row, a link or a tag back and only the next press
      // reaches the surface holding it.  The sheet is the floor — the panel names
      // no `off', so ESC from nav falls through to it — and a stray focus is what
      // is left under that.  The surfaces are mutually exclusive in practice (each
      // is raised from a table key, and `typing()' has already killed every one of
      // those by the time another is up), so the ORDER decides nothing a reader can
      // reach; it is the list's, and the list's order is the listeners'.
      cancel: () => {
        for (const s of SURFACES) {
          if (s.edit && s.edit()) { s.shut(); return; }
          if (s.off && s.up()) { s.off(); return; }
        }
        if (activeSheet()) leaveSheet();
        else if (typing()) active().blur();
      },
      // The filter's own backspace: the renderer drops the token and the
      // shell follows it — one commit, one URL, focus left on the table.
      //
      // A LADDER, in three rungs, and the rhyme is the backspace's: ERASE
      // THE LAST STRUCTURE STANDING.  A MARKED SET is one, so while there
      // are marks DEL takes them off and stops — the marks alone, since a
      // FLAG is the archive queue and a backspace must not empty it.  Then
      // the query's last TOKEN, as it always has.  Then, when the strip
      // EMPTIES the query and there is a trail behind it, the same key walks
      // back out of the drill that built the view — it applies the crumb's
      // query INSTEAD of the empty one, so `@' and `DEL' are one step out
      // and one step back rather than a step and a half.  A rung with
      // nothing under it falls through in silence; only the rung that RUNS
      // speaks.
      filterDrop: (b) => {
        if (clearMarking(named(b, "unmark-all"), false)) return;
        if (!strips()) { said(b, "this table-view.js has no filter tokens"); return; }
        if (!table.stripLastToken()) { said(b, "no filter"); return; }
        const left = table.getQuery().trim();
        if (!left && crumbing() && trail().length) {
          // The row this crumb was pushed from, when the side table is
          // still in step with the trail the renderer is holding.
          const sel = selsFit() ? crumbSels.pop() : null;
          const back = table.popCrumb();
          // The view being left takes its label with it; a crumb further
          // down the trail keeps its own, since the map is keyed by token.
          delete crumbLabels[query];
          applyView(b, back.query, () => said(b, `back to ${back.label}`), sel);
          return;
        }
        commit(left);
        said(b, left ? `filter: ${JSON.stringify(left)}` : "filter cleared");
      },
    };
    // The row is handed to its handler: one that names what it landed on
    // — the filter left, the column arrived at — echoes over this line with
    // the same `seq → command' opening.
    function run(b) {
      echo(`${b.seq} → ${b.command}${b.help ? ` · ${b.help}` : ""}`);
      const handler = b.handler && HANDLERS[b.handler];
      if (handler) handler(b);
      else append("cmd", "info", `${b.seq} (${b.command}) — arrives with daemon commands (M4)`);
    }
    document.addEventListener("keydown", (e) => {
      // A KEY ANOTHER LISTENER HAS ALREADY CLAIMED IS NOT THIS MAP'S, and the
      // document is the one surface that can hand a key back mid-press: its
      // listener runs AHEAD of this one, and `DEL' there closes the sheet — so by
      // the time this ran, `typing()' had gone false and the table's own `DEL'
      // would strip a filter token off the view underneath.  `defaultPrevented'
      // is the DOM's own word for handled, what every listener on this page
      // already says by calling `preventDefault'; the three that run BEHIND this
      // one are unaffected, since a row it claims it also runs.
      if (e.defaultPrevented) return;
      const k = keyName(e);
      if (!k) return;
      const keys = pending.concat([k]);
      const here = MAPS.rows.filter(live);
      // A row is in play while its keys open with the ones typed so far.
      const opens = (b) => keys.every((key, i) => b.keys[i] === key);
      const hit = here.find((b) => b.keys.length === keys.length && opens(b));
      // A held key still belongs to this map — it is claimed either way —
      // but a destructive one runs once per press.
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
    // The prompt's own keys, behind the dispatch above and safe for the reason
    // stated at the sheet's listener.  C-n and C-p are reserved chords the map
    // never claims, and claiming them HERE is the palette's business rather than
    // the map's — the same way a focused select keeps its arrows.
    //
    // Letter mode is bare letters only: `keyName' spells a chord `C-t' and a held
    // shift `T', neither a claimed letter, so both fall through to whatever else
    // wants them.  `keyName' names the press here too, so the which-key letters
    // are PHYSICAL keys the way the map's are — the pool is a-z by construction
    // (`whichKeys'), and a Cyrillic press arrives already spelled in that
    // alphabet.
    //
    // `raising' AND EXCLUSIVITY ARE DIFFERENT RULES, which is why `sole' does not
    // absorb it: exclusivity is one surface closing ANOTHER at the door, where
    // `raising' is this surface declining the one keydown that RAISED it — `t' is
    // both the listOpener and a letter in what it opens, and this listener sits
    // behind the dispatch, so that press arrives here next.  Only one surface is
    // involved, so no ordering between surfaces could say anything about it.
    document.addEventListener("keydown", (e) => {
      if (!prompting) return;
      if (prompting.raising) { prompting.raising = false; return; }
      const k = keyName(e);
      // A bare modifier spells no key, and an unbound entry claims no letter:
      // without this the two nulls would meet and Shift would commit whatever
      // came out of the pool empty.
      if (!k) return;
      // The mode that holds a LINE rather than a list (`askText'): RET takes the
      // line as typed and every other key is the field's own, with nothing to
      // narrow and no letter to commit.  A palette whose typing reaches past its
      // list takes the line as an ENTRY (`freely'), one with no list as text.
      if (prompting.text) {
        if (k !== "RET") return;
        takeChoice(freely() || { text: el("pinput").value });
        e.preventDefault();
        return;
      }
      // A letter writes, so it runs once per press — the `ONCE' rule, owed here
      // rather than by the map because the key that OPENS this palette is a letter
      // too, and a held one would raise it and commit through it.  The repeat is
      // claimed either way, the way the dispatch claims one it declines to run.
      // DEL arrives here as an ordinary entry key, `*empty*' holding it as its own;
      // a palette with no such entry — the tag one — leaves the press to nobody,
      // `typing()' having already killed the map's own DEL.
      if (!prompting.narrow) {
        const hit = prompting.choices.find((c) => c.key === k);
        // The fallback's own foot, named where the fallback is entered: the
        // letters are gone and the field's keys take their place.
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
    // A MOMENTARY POPUP'S KEYS, the two that BROWSE A MOUNT sharing the whole
    // shape: stand down unless this surface is the one up, name the key, hand it
    // to the open EDIT where there is one, then row movement, then the popup's
    // own chain, and claim whatever landed.  Written once, so a third popup is a
    // declaration rather than a fourth listener to keep in step.  Registered
    // BEHIND the dispatch, safe for the value palette's reason: while a popup is
    // up `typing()' has already made every `table' row dead, so the only row that
    // can have fired ahead is `ESC' — which is the one that should, `cancel'
    // closing whichever overlay is up.
    //
    // TWO ASYMMETRIES, both declared rather than flattened away.
    // `defaultPrevented': a key another listener has already CLAIMED is not this
    // one's — the tags popup can have a field raised over it (`+'), whose
    // listener runs ahead of this and closes the overlay as it commits, so the
    // very `RET' that added a tag would arrive here and open the rename.  It is
    // asked of BOTH now, the link popup raising no field today and "handled is
    // handled" being a rule no one surface should keep for itself.  `e.repeat':
    // a key that WRITES runs once per press, spelled in the chain that owns it
    // rather than lifted here — the tags popup's `d'/`D'/`u' are the deletion
    // gesture, where a repeat that survived would flag a tag and remove it from
    // ONE press.
    function popupKeys(name, mount, o) {
      document.addEventListener("keydown", (e) => {
        if (momentary() !== name || e.defaultPrevented) return;
        const k = keyName(e);
        if (!k) return;
        if (o.editing()) { if (!o.editKeys(k, e)) return; }
        else {
          const step = rowStep(k);
          if (step) stepIn(mount(), step);
          // DEL ERASES THE LAST STRUCTURE STANDING, the backspace's rhyme everywhere
          // on this page: over the table it takes the marks, then the query's last
          // token, then a rung off the drill trail.  Over a popup the popup IS that
          // structure — neither of these has an inner ladder — so the key closes it,
          // through the same `off' ESC reaches and read off `SURFACES' rather than
          // named a second time here.  IN NAV ALONE: inside an open edit the key is
          // the FIELD's own erase, the edit branch above declining it, and a key this
          // listener declines is one it does not `preventDefault', which is the whole
          // of what leaves it to the field.
          // `q' IS THE OTHER DOOR OUT, and it is dired's: a browsing surface
          // closes on it.  Same rung and same `off' as DEL above, and the same
          // exception — the value palette keeps its letters, `q' there being a
          // keyword's initial like any other.
          else if (k === "DEL" || k === "q") {
            // The surface `momentary()' just named, asked for by that name: the
            // `|| {}' this used to fall back to would have thrown on a miss,
            // and a surface with no `off' is one ESC falls through from.
            const surface = SURFACES.find((s) => s.name === name);
            if (surface && surface.off) surface.off();
            keySaid(k)("keyboard-quit");
          }
          else if (!o.keys(k, e)) return;
        }
        e.preventDefault();
      });
    }
    // MOVE, LOOK, OPEN — the whole of the link popup today.  Row movement is
    // `rowStep', the property panel's own: both spellings and the arrows, bound
    // unconditionally the way the panel's are, the popup holding no field and
    // every printable key being free.
    //
    // `o' is the OPEN key, the key that raised this — the table's own `o' carried
    // inside, over the link the cursor is on rather than over the row.  It opens
    // and CLOSES, both outcomes alike (the tab and the refusal), picking one link
    // being what the popup was raised to do and a popup that stayed up on the
    // refusal being a second rule for the same key.
    //
    // `RET' EDITS the link at point in place — the row's own title and url cells
    // becoming fields over themselves, `TAB' between them, `RET' committing and
    // `ESC' restoring, which is the property panel's edit model exactly.  ONE
    // edit vocabulary across the page: a panel row, a tag and a link are edited
    // alike, and the derived cell — a coverage, a count, a link's type — never
    // opens.
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
    // MOVE, RENAME, FLAG, REMOVE, ADD — the same shape, one popup over.  `RET'
    // opens the rename and, with the overlay up, commits it; `d'/`D'/`u' are the
    // deletion gesture, spelled here as on the other three surfaces and guarded
    // against a HELD key the same way; `+' raises the add field OVER this popup,
    // the one raise `sole' exempts and the reason the guard above exists.
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
      // A WRITE COMES BACK THROUGH THE WATCH, for the sheet as for the table. The
      // command route never writes the store, so a `set-state' or a `set-title'
      // made from the document leaves the sheet holding what the file said BEFORE
      // it; the frame naming this row is when there is something fresher to read.
      // Never while an edit is open, a re-read pulling the model out from under
      // it.
      const moved = frame.op === "delete-row" ? frame.id : (frame.row || {}).id;
      // NEVER OVER UNCOMMITTED WORK, AND NEVER UNDER THE READER'S HANDS.
      // `reload' rebuilds both panes — `prows' is rebuilt, `baseProps' re-pinned
      // and `drawProps' clears `#mprops.on' — so a re-read while the reader has
      // an open panel row, a committed drawer edit they have not flushed, or
      // merely the panel's CURSOR, throws that away silently and under a `synced'
      // header; `pnav()' is in the guard because losing the keys back to the
      // document pane mid-read is the same theft one grain smaller.  And the
      // reader's own `t', `:' or `S-<up>' from inside the sheet is what CAUSES
      // the event, so that is the ordinary case rather than a race.
      if (editing && !raw && !sheetOpen() && !dirty() && !pnav()
          && moved === editing.id)
        reload();
      if (!table) return;
      // Under a filter the loaded rows are the server's answer to a query,
      // and only it knows whether the changed row still matches: ask again.
      // The refetch is where the rows leave for a filtered client, so it is the
      // refetch that has to land the archive's anchor — hence `settled' rather
      // than the first-row landing every other caller of `fetchRows' takes.
      if (query) return void (clearTimeout(requeryAt),
        requeryAt = setTimeout(() => fetchRows(settled), 250));
      if (frame.op === "upsert-row") table.upsertRow(frame.row);
      else if (frame.op === "delete-row") table.deleteRow(frame.id);
      // And the splice is where they leave for an unfiltered one.  The renderer
      // has already kept the cursor by the time this runs — on its row while that
      // row is there, else at the same visual place — so this only ever overrides
      // that with the anchor, and only for the frame taking point's own row out.
      else return;
      settled();
    }
    function listen() {
      const scheme = location.protocol === "https:" ? "wss" : "ws";
      // The rows came over HTTP; the socket's own set-rows would resend them.
      socket = new WebSocket(`${scheme}://${location.host}/ws?bootstrap=off`);
      // The other half of the wash, and the only one a reader can sit in for
      // minutes: a page whose socket is gone goes on showing rows nothing can
      // correct.  Set rather than stepped — a connection refused closes without
      // ever opening — and the delay keeps a reconnect that costs one
      // revalidation from dimming anything.
      socket.onopen = () => {
        backoff = 1000; wash.want("socket", 0);
      };
      socket.onmessage = (e) => apply(JSON.parse(e.data));
      socket.onclose = (e) => {
        socket = null;
        wash.want("socket", 1);
        // The columns moved, which SCHEMA.md's row ops cannot say: the
        // mount has to go.  Every other close — a backlog abandoned under
        // a write storm (`resync'), a restarted daemon, a dead network —
        // costs rows and nothing else, and the page stays where it was.
        if (e && e.reason === "view-changed") remount(); else resync();
      };
    }
    // A lost socket costs rows and keeps the page.  Ask
    // /headlines for the applied query with the tag the last answer carried:
    // an unmoved store answers 304 and costs a header exchange, a moved one
    // answers with rows that drop into the table standing here.  The mount
    // stays through both — the sheet, the palette, the selection and the URL
    // with it — which is what makes an editor's write storm a row refresh
    // rather than the page reloading under a reader's hands.
    function resync() {
      if (!table) { start(); return; }   // nothing mounted yet: this is a boot
      const asked = query;
      load(asking(asked), etag).then((a) => {
        // The close reason is not trusted for this: a daemon restarted while
        // this page was away had no socket to send `view-changed' down, and
        // its columns can still have moved.
        if (a.view && !sameColumns(a.view.columns || [])) { remount(); return; }
        // A repaint of the SAME view is a third road the archive's rows can leave
        // by: the write landed while the socket was down and the reconnect's
        // answer is the first the page has seen without them.
        if (a.view && query === asked) { paint(a); settled(); }
        backoff = 1000;
        listen();
        append("ws", "info", a.view ? "reconnected · rows refreshed" : "reconnected");
      }).catch((e) => {
        if (e.indexing) return indexing(e.indexing);
        // A newer query is already fetching and will paint what it gets;
        // the socket is all this call still owed.
        if (e.name === "AbortError") { listen(); return; }
        quiet(e); again();
      });
    }
    // The columns are the one part of a view rows cannot carry, so they are
    // compared whole: the state column's badge palette rides inside them,
    // and a key-by-key check would let it move unnoticed.
    const sameColumns = (next) => JSON.stringify(next) === JSON.stringify(cols);
    function again() {
      append("ws", "warn", `disconnected · retrying in ${Math.round(backoff / 1000)}s`);
      setTimeout(resync, backoff);
      backoff = Math.min(backoff * 2, 30000);
    }
    // The server binds before it walks the tree, so the first fetch of a
    // cold daemon is a 503: show what it is doing and ask again in a second.
    // A daemon that restarts under a live page lands here too, and comes
    // back through `resync' — the page it left is still on screen.
    function indexing(b) {
      append("boot", "info", `indexing … ${b.elapsed}s · the table opens when the walk lands`);
      setTimeout(resync, 1000);
    }
    // AFTER is what a canned view wants doing once its own rows are up, given the
    // server's match count.  An argument rather than a variable this arms and
    // disarms, so it belongs to the boot it was passed to and a boot that never
    // lands cannot leave one behind for the next.  It also carries the LANDING,
    // which is why a caller that passes one lands nothing here: a pop puts the
    // cursor back on the row its drill was launched from, and this door must not
    // land row one over it first.
    function start(after) {
      // A `?q=' in the address bar is a filtered view, and so is a bare
      // boot: the boot asks for whichever it is and `mount' opens the
      // filter showing it.  Every return through this door — a reload,
      // `view-changed', `g' — restores it the same way, since they all
      // re-fetch and re-mount; a reconnect never comes here at all.
      // The default is written into the URL where it was injected, so what
      // the page shows and what the address bar says are the same query
      // from the first paint on.
      const asked = (query = bootQuery());
      if (!params().has("q")) remember(asked);
      // SWAP ON THE ANSWER.  A boot has nothing on screen, so it takes the first
      // page it can get and pulls the rest in behind the painted table.  A
      // RE-APPLICATION has a whole table standing — `g', `a', `@', a pop, a
      // `view-changed' remount — and asks for the WHOLE answer once, a page-sized
      // mount here replacing a complete table with a partial one and reflowing
      // the pager and the hint under the reader a moment later.  Under either,
      // the rows that are up STAND until the new ones are in hand and the swap is
      // one mount; the wash is what says they are on their way.
      const swap = !!table;
      const narrow = asking(asked) + (asked ? "&" : "?");
      viewing(load(swap ? asking(asked) : `${narrow}limit=${PAGE}`)).then((a) => {
        mount(a.view);
        // A MOUNT LANDS, and a BOOT IS AN APPLIED VIEW.  A new mount has no cursor
        // of its own — the renderer selects nothing until something asks it to — so
        // a page that landed nothing here would open with `d', `D' and `RET' all
        // answering `no row' until the reader pressed `n'.  It is the apply landing
        // and goes through `land' like every other, so row one is spelled in
        // exactly one place.  A caller with an opinion — a pop, through `applyView'
        // — lands inside AFTER instead and this one stands aside for it.
        if (after) after(a.total); else land(null);
        listen();
        // The full set arriving behind the first page LANDS NOTHING: the cursor
        // this just put on row one is the reader's from the first paint on, and
        // `paint' keeps it the way the renderer keeps every selection — on its row
        // while the row is there.  One landing per mount, at the mount.
        // The rest behind the painted table: n/p, sort and materialize all
        // want the whole answer, and the renderer holds it without the DOM.
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
    // The first line of the log, and an ordinary one: the strip is never
    // cleared, so the boot stays in the scrollback under everything that
    // follows it rather than being a placeholder something has to take away.
    append("boot", "info", "loading …");
    start();
