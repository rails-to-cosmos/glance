    // THE LINK POPUP, a table-view MOUNT — the page's third, after the table and
    // the sheet's property panel.  What a row points at is a LIST OF RECORDS
    // rather than a set of commands: each link has a kind, a name and a
    // destination, and reading them is how a reader decides which one they meant.
    // A which-key letter is the right shape for a fixed vocabulary committed from
    // memory (a keyword, a tag) and the wrong one for a list you have to READ,
    // where the letters are noise over the columns that carry the answer.  So
    // this one browses: move, look, RET.
    //
    // READ-ONLY, and stated rather than inherited: no marks, no flags, no
    // pageSize and no hint line.  Nothing here writes, so a gutter, a wash and a
    // per-row hint would each be chrome about a gesture the popup does not have.
    // `palette: true' for the property panel's reason — a handful of links is
    // not something a reader narrows, and the filter overlay it leaves behind is
    // never raised.  Mounted once and kept, like the panel: a mount per press
    // would leave a theme listener behind every time the reader followed a row.
    const LCOLS = CFG.lcols;
    // `opening' is the BINDING that raised this — `o' or `!' — and it is also
    // WHETHER the popup is up: a raise sets it, `shutLinks' clears it, and one
    // value answers both questions rather than two that have to move in step.
    // It is what lets the pill name the command that ran when the commit finally
    // lands, the way it does for a link the row held only one of.
    let lmount = null, lrows = [], opening = null, lfor = null, lpin = "";
    const linking = () => !!opening;
    function linksMounted() {
      if (lmount) return lmount;
      lmount = mountOnce("ltable", LCOLS,
        { palette: true, marks: false, flags: false, actionHints: false },
        "lpane");
      return lmount;
    }
    // The answer as rows, under B — the binding that asked — for the row ID.
    // The link's own id is its PLACE in the answer, the only identity a link has
    // here: two entries may share a description, and the targets are deduplicated
    // upstream.  The model keeps it beside the link, so finding the link the
    // cursor is on is a lookup by id like the property panel's rather than this
    // page taking an id apart.  TWO THINGS BESIDE THE ROWS make `RET' a WRITE:
    // the row the links belong to, since a command names rows and this popup is
    // raised over one, and the answer's DIGEST, the file as the store read it
    // when it measured these spans.  An edit pins that digest, so a file that has
    // moved since refuses rather than splicing a range that has.
    function showLinks(b, id, answer) {
      sole();
      const links = answer.links || [];
      lrows = links.map((l, i) => ({ id: `L${i}`, link: l }));
      lfor = id;
      lpin = answer.digest || "";
      const m = linksMounted();
      m.setRows(lrows.map((r) => ({ id: r.id,
        cells: { type: r.link.type, title: r.link.desc, url: r.link.target } })));
      showPopup("links", "l", `open · ${links.length} links`,
                "RET edits · o opens it · ESC leaves");
      opening = b;
      if (lrows.length) m.select(lrows[0].id);
    }
    // An open edit is a rung UNDER the popup, and closing the popup takes it
    // with it; nothing else is focused — the popup holds the keys with no field
    // in it, the way the property panel's nav does — so the rest of closing is
    // the class coming off.  BOTH popups shut alike, so that pair is one
    // function and each caller adds only the state whose emptiness IS its
    // `up()': `opening' for the links, `tagging' for the tags.
    function shutPopup(id, shape) { shutEdit(shape); el(id).className = ""; }
    // And RAISED alike: the head takes the words this raise gives it, the foot
    // takes them where the surface has one to say, and the class goes on.  P
    // names the parts (`lhead'/`lfoot', `thead'/`tfoot'), so a third popup
    // joins by naming its own prefix.  `sole()' stays each caller's FIRST line
    // and not this function's: it closes every momentary surface including the
    // one being raised, so run from here it would wipe the state the caller has
    // just written.
    function showPopup(id, p, head, foot) {
      el(p + "head").textContent = head;
      if (foot !== undefined) el(p + "foot").textContent = foot;
      el(id).className = "on";
    }
    function shutLinks() {
      shutPopup("links", LROW);
      opening = null; lfor = null; lpin = "";
    }
    // The row the cursor is on, out of the renderer's own selection — this page
    // keeps no copy of where a popup is standing, the same rule the table and the
    // panel follow.  The ROW rather than the link, the overlay opening over the
    // row and the link being what it holds.
    function pointedRow() {
      const at = selectedId(lmount);
      return lrows.find((r) => r.id === at) || null;
    }
    const pointedLink = () => (pointedRow() || {}).link || null;
    // THE LINK OVERLAY: `openEdit' over TWO cells.  The description and the
    // target become fields over themselves, `TAB' hops, `RET' commits and `ESC'
    // restores — the property panel's edit model exactly, and the third surface
    // to declare a shape for it.  The type column is DERIVED (the server's word
    // for the target, which the write itself may move), so it never opens and the
    // box covers the two cells that can.  The target takes the focus, for the
    // panel's reason: editing a link already there is nearly always editing where
    // it points.
    const LROW = {
      box: "ledit", pane: "lpane", fields: ["ltitle", "lurl"],
      cells: ["title", "url"], cols: LCOLS,
      mount: () => lmount,
      fill: (r) => {
        el("ltitle").value = r.link.desc;
        el("lurl").value = r.link.target;
      },
      focus: () => { el("lurl").focus(); el("lurl").select(); },
    };
    const lediting = () => !!edit && edit.o === LROW;
    // RET OVER NOTHING SAYS SO, and both popups say it the same way: the row the
    // cursor is on or the command's own name for having none.  One guard, so a
    // surface that grew an overlay cannot forget the empty case.
    const openOver = (shape, at, none) =>
      (at ? openEdit(shape, at) : echo(`RET → ${none}`));
    const openLinkEdit = () =>
      openOver(LROW, pointedRow(), "org-insert-link (no link)");
    const cancelLinkEdit = () => cancelEdit("link", LROW);
    // THE COMMIT, ONE row and ONE SPAN: the range `/links' handed out, spliced
    // under the digest that answer carried.  The link is the one the overlay
    // OPENED over — the snapshot every surface on this mechanism gets — so a
    // click that moved the cursor under an open field cannot redirect the write.
    //
    // The popup CLOSES on the press, both outcomes alike, which is `o'\''s own
    // rule: picking one link is what it was raised to do.  It also has to — the
    // spans it is holding describe the file as it was and the write has just
    // moved it, and the store does not know yet either (`/command' never writes
    // it, the watch does, a debounce later), so a popup left standing would be
    // offering ranges into a text that is gone and a re-read here would answer
    // with what the file said BEFORE the write.  `o' again is one keystroke and
    // comes back with fresh spans.
    //
    // ABSENT IS NOT NULL.  The description field opens on what the link SHOWS,
    // which for a link carrying none of its own is its target, so sending that
    // back unchanged would spell the target into a description.  Only a field
    // the reader MOVED says anything: left alone it is absent and the link keeps
    // what it has, emptied it is the null that takes the description off.
    function commitLink(row) {
      const link = row.link;
      const target = String(el("lurl").value).trim();
      const typed = String(el("ltitle").value).trim();
      const b = opening, id = lfor, pin = lpin;
      shutLinks();
      if (!target) { said(b, "a link points somewhere"); return; }
      const args = { span: link.span, target };
      if (typed !== link.desc) args.desc = typed || null;
      if (target === link.target && args.desc === undefined)
        { said(b, "unchanged"); return; }
      fire(b, "edit-link", [id], args, `link edited: ${shortly(link.target)} → ${shortly(target)}`, null,
           { [id]: pin });
    }
    // THE TAGS POPUP, the page's FOURTH table-view mount and the first one that
    // WRITES.  What a set of rows is tagged with is a list of RECORDS — a name, a
    // coverage over the set, a weight in the tree — and a reader deciding whether
    // to drop one is READING those three: the link popup's shape rather than the
    // value palette's, so the which-key letters that used to carry this list went
    // with the list, a letter being right for a fixed vocabulary committed from
    // memory, which is what a KEYWORD is and a tag over a set of rows is not.
    //
    // MUTABLE, which is the whole of what makes it a fourth mount rather than a
    // second link popup.  `d'/`D' remove, `+' adds, `RET' renames — three
    // gestures this page already spells elsewhere, borrowed rather than
    // invented: dired's flag-then-confirm from the table and the property panel,
    // the value palette's completing field, and the panel's edit overlay.
    //
    // Marks are OFF: the set a tag command runs over is the TABLE's and was
    // decided before this went up, so a second selection here would be a second
    // answer to a settled question.  Flags are ON, the removal being the
    // two-press gesture and the flag its confirmation.
    const TCOLS = CFG.tcols;
    // The popup's whole state: the target rows as the server described them (each
    // with its own folded tag list), the tree's vocabulary and its store-wide row
    // counts, and the binding that raised this — which is also WHETHER it is up,
    // the way `opening' is for the link popup.  What the mount is SHOWING is not
    // among them: a tag is its own row id, so `tagUnion()' answers every question
    // a copy of the row list could, and a copy is one more thing each write has
    // to remember to refresh.
    let tmount = null, ttargets = [], tvocab = [], tcount = {};
    let tagging = null;
    const managing = () => !!tagging;
    // Mounted once and kept, for the panel's reason.
    function tagsMounted() {
      if (tmount) return tmount;
      tmount = mountOnce("ttable", TCOLS,
        { palette: true, marks: false, flags: true, actionHints: false,
          flagHelp: "d/D remove · u unflag" },
        "tpane");
      return tmount;
    }
    // THE UNION over the target rows, FIRST-SEEN: each row's tags in the order
    // its file spells them, the rows in the order the server named them.
    // Alphabetical would be no harder and strictly worse — the cursor sits on a
    // row, and an insert in the middle moves the row out from under it, where an
    // append cannot.
    function tagUnion() {
      const seen = [];
      for (const r of ttargets) for (const t of r.tags)
        if (seen.indexOf(t) === -1) seen.push(t);
      return seen;
    }
    // Which of the targets carry TAG, which is what every write here is aimed
    // at: a removal goes to the rows carrying it, an add to the rows lacking it,
    // so what an answer counts is rows that MOVED.
    const carriers = (tag) => ttargets.filter((r) => r.tags.indexOf(tag) !== -1);
    // COVERAGE: `all' where every target carries it, `k/n' where some do.  It is
    // what the letter palette wrote into a muted aside, promoted to a column of
    // its own now that there is a table to put it in.
    const coverage = (tag) => {
      const on = carriers(tag).length;
      return on === ttargets.length ? "all" : `${on}/${ttargets.length}`;
    };
    // One tag as the mount holds it, and a tag IS its own id: one entry per tag
    // per popup, so a flag, the cursor and a rename all name the same thing
    // after any number of writes.
    const tagRow = (tag) =>
      ({ id: tag, cells: { title: tag, on: coverage(tag),
                           rows: tcount[tag] === undefined ? "" : tcount[tag] } });
    // Every change to the model ends here.  AT is the tag to land the cursor on
    // and is left out where it should stay where it is.  The union is read once
    // and the three answers below are folds over it.
    function repaintTags(at) {
      const m = tagsMounted();
      const tags = tagUnion();
      m.setRows(tags.map(tagRow));
      // The foot names what a reader can do, and a popup with nothing in it
      // names the one key that still can: an untagged set is honest rather than
      // empty, and `+' is the way in.
      el("tfoot").textContent = tags.length
        ? "RET renames · d flags · D removes · + adds · ESC leaves"
        : "nothing tagged here · + adds one · ESC leaves";
      if (at && tags.indexOf(at) !== -1) m.select(at);
    }
    // Raised on the ANSWER, like the link popup and unlike the state palette: no
    // key inside this list is also the key that opens it, so nothing is gained by
    // putting an empty mount up first and no raising guard is owed.  TITLE is the
    // count of the ids the command was aimed at, which is what the reader asked
    // for; the coverage denominator is the rows the store actually answered for.
    function showTags(b, title, answer) {
      sole();
      ttargets = (answer.rows || []).map((r) =>
        ({ id: r.id, tags: (r.tags || []).slice() }));
      tvocab = answer.vocabulary || [];
      tcount = answer.counts || {};
      tagging = b;
      // Written ONCE, here: the title is the count of the ids the command was
      // aimed at and cannot move while the popup is up, so a repaint has no
      // business restating it.
      showPopup("tags", "t", title);
      repaintTags(tagUnion()[0]);
    }
    // Nothing to blur once the rename is shut: the popup holds the keys with no
    // field in it, the way the link popup and the property panel's nav do.
    function shutTags() {
      shutPopup("tags", TROW);
      tagging = null; ttargets = [];
    }
    // The tag the cursor is on, out of the renderer's own selection, by the rule
    // the table, the panel and the link popup follow.  Checked against the UNION,
    // what the mount was drawn from: a selection the popup no longer holds a row
    // for — a shut popup's included, `ttargets' emptying the union — is no tag.
    const tagAt = () => {
      const at = selectedId(tmount);
      return tagUnion().indexOf(at) !== -1 ? at : null;
    };
    // THE ADDABLE VOCABULARY, what `+' completes over: every tag this tree holds
    // LESS the ones already on every target.  Adding one of those writes nothing,
    // so offering it is offering a no-op — where one only SOME of them carry
    // stays offered, wearing its `2/3', since adding it levels the set up.  The
    // set's own partial tags lead and the rest of the TREE follows: the rows a
    // page shows are a fraction of the store, so the vocabulary is the server's
    // answer rather than a scan of what is in hand.
    function addable() {
      const union = tagUnion();
      // `all' is exactly the coverage of a tag every target carries, so the
      // filter reads that cell rather than counting the carriers a second time.
      return union.map((t) => ({ label: t, tag: t, hint: coverage(t) }))
        .filter((c) => c.hint !== "all")
        .concat(tvocab.filter((t) => union.indexOf(t) === -1)
          .map((t) => ({ label: t, tag: t, hint: "" })));
    }
    // The ids one answer landed on, which is where every fold below starts.
    // What the list shows next comes out of the command's OWN per-id answer,
    // never a re-read: `/command' does not write the store — the watch does, a
    // debounce later — so asking `/tags' again here would answer with what the
    // files said BEFORE the write.
    const landedIds = (results) =>
      new Set((results || []).filter((x) => x.ok).map((x) => x.id));
    // And the store-wide count stepped by what landed.  Arithmetic on the answer
    // for the same reason the tag sets are, and corrected by the next
    // resolution: the number is the tree's and only the tree can be right about
    // it, but a column standing still while the rows under it moved would read
    // as a stale answer rather than as a different question.
    const stepCount = (tag, by) =>
      (tcount[tag] = Math.max(0, (tcount[tag] || 0) + by));
    // WHAT EVERY TAG WRITE DOES WITH ITS ANSWER, in one place: a popup the reader
    // has already left is dropped, the ids that landed are folded into the model
    // by APPLY — which is the whole of what the three commands differ in — and
    // the list is redrawn with the cursor on AT.  Three copies of that frame is
    // three chances to forget the guard, and forgetting it writes rows into a
    // popup that is not there.
    const landing = (at, apply) => (results) => {
      if (!managing()) return;
      apply(landedIds(results));
      repaintTags(at);
    };
    // `+' — the add flow, one field over the addable vocabulary and the only door
    // into it.  It only ever ADDS, so the write goes to the rows LACKING the tag;
    // a tag every target already carries costs a line in the pill and no round
    // trip.  FOLDED at commit, because presence is: `/tags' reports what
    // `tagsOfCell' reads, and a popup that wrote `Work' would go on showing
    // `work' and offering to add it again.
    const addFlow = () => askFrom(`add a tag · ${rowsWord(ttargets.length)}`,
      addable(), "RET adds it · C-n/C-p walks · ESC leaves", addTag);
    function addTag(c) {
      const tag = tagFrom(c);
      if (!managing() || !tag) return;
      const over = ttargets.filter((r) => r.tags.indexOf(tag) === -1);
      if (!over.length) { said(tagging, `:${tag}: is on every row already`); return; }
      fire(tagging, "add-tag", over.map((r) => r.id), { tag },
           `tagged :${tag}:`).then(landing(tag, (landed) => {
        for (const r of ttargets)
          if (landed.has(r.id) && r.tags.indexOf(tag) === -1) r.tags.push(tag);
        // A tag written for the first time joins the tree's vocabulary here, so
        // the field offers it before the watch has told this page anything.
        if (landed.size && tvocab.indexOf(tag) === -1) tvocab.push(tag);
        stepCount(tag, landed.size);
      }));
    }
    // `D', and the second `d' that reaches the same handler: every FLAGGED tag
    // comes off every target carrying it.  ONE COMMAND PER TAG, a command naming
    // one — each its own per-file batch of atomic writes.  The flags were spent
    // before this ran (`flagKey'), and the count the other two surfaces word is
    // nothing this can use, so it takes the ids and drops it.  AWAITED, for
    // `cyclePriority''s reason: two tags coming off rows that share a FILE are
    // two requests against one drift lock, and fired together each is measured
    // against a digest the other is moving.  Guarded for its other reason too — a
    // refusal on one tag must not abandon the tags behind it.
    async function removeTags(list) {
      for (const tag of list)
        await Promise.resolve(untag(tag)).catch(failed(tagging, "remove-tag"));
    }
    function untag(tag) {
      const over = carriers(tag);
      if (!over.length) return;
      return fire(tagging, "remove-tag", over.map((r) => r.id), { tag },
           `untagged :${tag}:`).then(landing(null, (landed) => {
        for (const r of ttargets)
          if (landed.has(r.id)) r.tags = r.tags.filter((t) => t !== tag);
        stepCount(tag, -landed.size);
      }));
    }
    // `RET' — the rename, and ONE command.  `rename-tag' replaces the entry where
    // the author put it, under one drift lock per file; a remove and an add fired
    // in turn would be two writes, two locks, and a tag that moved to the end of
    // every run it was in.  It goes to the targets carrying FROM, the set the
    // write is for.  FROM is the tag the overlay OPENED over, read out of the
    // snapshot before the overlay comes down: a click that moved the cursor under
    // an open field must not rename the tag the reader landed on.
    function renameTag(from, typed) {
      const to = foldTag(typed);
      shutEdit(TROW);
      if (!from || !to || to === from) { said(tagging, "unchanged"); return; }
      const over = carriers(from);
      fire(tagging, "rename-tag", over.map((r) => r.id), { from, to },
           `renamed :${from}:→:${to}:`).then(landing(to, (landed) => {
        // A row carrying BOTH ends loses `from' and gains nothing — the server
        // cuts rather than renames there — so counting it would leave the tree
        // count one high for as long as the popup stands.
        const gained = ttargets.filter((r) =>
          landed.has(r.id) && r.tags.indexOf(to) === -1).length;
        for (const r of ttargets)
          if (landed.has(r.id)) r.tags = renamedTags(r.tags, from, to);
        if (landed.size && tvocab.indexOf(to) === -1) tvocab.push(to);
        stepCount(to, gained);
        stepCount(from, -landed.size);
      }));
    }
    // One row's tags after the rename, IN PLACE and deduplicated — the server's
    // own rule ('Glance.Query.renameTagEdits'): the entry stays where it was, so
    // the union's first-seen order does not shuffle under the cursor, and a row
    // that carried both ends comes out carrying one.
    const renamedTags = (tags, from, to) =>
      [...new Set(tags.map((t) => (t === from ? to : t)))];
    // THE RENAME OVERLAY: `openEdit' over one CELL.  The tag cell becomes a field
    // over itself, `RET' commits and `ESC' restores.  The other two columns are
    // DERIVED — a coverage and a count — so there is nothing in them to edit and
    // they never open, exactly as the link popup's type cell does not; `cells'
    // says which of them the box covers.  The tag the overlay opened on is
    // `edit.row', the snapshot every surface using this mechanism gets.
    const TROW = {
      box: "tedit", pane: "tpane", fields: ["tname"],
      cells: ["title"], cols: TCOLS,
      mount: () => tmount,
      fill: (tag) => (el("tname").value = tag),
      focus: () => { el("tname").focus(); el("tname").select(); },
    };
    const renaming = () => !!edit && edit.o === TROW;
    const openRename = () =>
      openOver(TROW, tagAt(), "org-rename-tag (no tag)");
    const cancelRename = () => cancelEdit("tag", TROW);
    // The popup's phrases for `flagKey', the gesture itself being the property
    // panel's.  `tagAt' already answers with an id or null, which is what the
    // shape asks for; `removeTags' names one tag per command and has no use for
    // the count the panel spells, so it takes the ids and drops it.
    const TFLAGS = {
      mount: () => tmount, at: tagAt, take: removeTags, note: unlogged,
      walk: () => stepIn(tmount, 1),
      missing: "this table-view.js has no delete flags",
      none: "org-toggle-tag (no tag)",
      unflag: "tag-unflag (flag cleared)",
      flag: "tag-flag (d again removes)",
    };
