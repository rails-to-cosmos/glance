// THE LINK AND TAGS POPUPS, and the popup chrome both wear
// (docs/proposal-widget-files.md, step C).  Twenty-two of its dependencies are
// `const'/`function' and destructure safely; `edit' is a `let' the panel
// reassigns, so it arrives as the ACCESSOR `editNow' -- a destructured `let' is
// a copy of whatever it held at boot.
const Popups = ((deps) => {
    const { CFG, again, askFrom, cancelEdit, echo, el, failed, fire, foldTag, listing,
            openEdit, remembered, rowsWord, said, selectedId, shortly, shutEdit,
            sole, soon, stepIn, tagFrom, unlogged } = deps;
    const editNow = deps.editNow;
    // The link and tags popups.  Full rules live in CLAUDE.md.
    const LCOLS = CFG.lcols;
    let lmount = null, lrows = [], opening = null, lfor = null, lpin = "";
    const linking = () => !!opening;
    function linksMounted() {
      if (lmount) return lmount;
      lmount = listing("ltable", LCOLS, "", "lpane");
      return lmount;
    }
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
    // Every raise and every close ends in the URL: `soon' so the surface's own
    // state has settled -- a closer clears its flag AFTER the class comes off.
    function shutPopup(id, shape) {
      shutEdit(shape); el(id).className = ""; soon(remembered);
    }
    // Callers call `sole()' first; from here it would wipe what they just wrote.
    function showPopup(id, p, head, foot) {
      el(p + "head").textContent = head;
      if (foot !== undefined) el(p + "foot").textContent = foot;
      el(id).className = "on";
      soon(remembered);
    }
    function shutLinks() {
      shutPopup("links", LROW);
      opening = null; lfor = null; lpin = "";
    }
    function pointedRow() {
      const at = selectedId(lmount);
      return lrows.find((r) => r.id === at) || null;
    }
    const pointedLink = () => (pointedRow() || {}).link || null;
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
    const lediting = () => { const e = editNow(); return !!e && e.o === LROW; };
    const openOver = (shape, at, none) =>
      (at ? openEdit(shape, at) : echo(`RET → ${none}`));
    const openLinkEdit = () =>
      openOver(LROW, pointedRow(), "org-insert-link (no link)");
    const cancelLinkEdit = () => cancelEdit("link", LROW);
    // Closes on the press: the spans describe the file this write just moved.
    // A `desc' nobody moved is absent; emptied it is the null that takes it off.
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
    const TCOLS = CFG.tcols;
    let tmount = null, ttargets = [], tvocab = [], tcount = {};
    let tagging = null;
    const managing = () => !!tagging;
    function tagsMounted() {
      if (tmount) return tmount;
      tmount = listing("ttable", TCOLS, "d/D remove · u unflag", "tpane");
      return tmount;
    }
    // First-seen order: an alphabetical insert would move rows under the cursor.
    function tagUnion() {
      const seen = [];
      for (const r of ttargets) for (const t of r.tags)
        if (seen.indexOf(t) === -1) seen.push(t);
      return seen;
    }
    const carriers = (tag) => ttargets.filter((r) => r.tags.indexOf(tag) !== -1);
    const coverage = (tag) => {
      const on = carriers(tag).length;
      return on === ttargets.length ? "all" : `${on}/${ttargets.length}`;
    };
    const tagRow = (tag) =>
      ({ id: tag, cells: { title: tag, on: coverage(tag),
                           rows: tcount[tag] === undefined ? "" : tcount[tag] } });
    function repaintTags(at) {
      const m = tagsMounted();
      const tags = tagUnion();
      m.setRows(tags.map(tagRow));
      el("tfoot").textContent = tags.length
        ? "RET renames · d flags · D removes · + adds · ESC leaves"
        : "nothing tagged here · + adds one · ESC leaves";
      if (at && tags.indexOf(at) !== -1) m.select(at);
    }
    // Raised on the answer: no key in this list opens it, so no raising guard.
    function showTags(b, title, answer) {
      sole();
      ttargets = (answer.rows || []).map((r) =>
        ({ id: r.id, tags: (r.tags || []).slice() }));
      tvocab = answer.vocabulary || [];
      tcount = answer.counts || {};
      tagging = b;
      showPopup("tags", "t", title);
      repaintTags(tagUnion()[0]);
    }
    function shutTags() {
      shutPopup("tags", TROW);
      tagging = null; ttargets = [];
    }
    const tagAt = () => {
      const at = selectedId(tmount);
      return tagUnion().indexOf(at) !== -1 ? at : null;
    };
    function addable() {
      const union = tagUnion();
      return union.map((t) => ({ label: t, tag: t, hint: coverage(t) }))
        .filter((c) => c.hint !== "all")
        .concat(tvocab.filter((t) => union.indexOf(t) === -1)
          .map((t) => ({ label: t, tag: t, hint: "" })));
    }
    // The model steps off the command's OWN answer: `/command' never writes the
    // store, so a `/tags' re-read here would answer with the pre-write files.
    const landedIds = (results) =>
      new Set((results || []).filter((x) => x.ok).map((x) => x.id));
    const stepCount = (tag, by) =>
      (tcount[tag] = Math.max(0, (tcount[tag] || 0) + by));
    const landing = (at, apply) => (results) => {
      if (!managing()) return;
      apply(landedIds(results));
      repaintTags(at);
    };
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
        if (landed.size && tvocab.indexOf(tag) === -1) tvocab.push(tag);
        stepCount(tag, landed.size);
      }));
    }
    // Awaited: two tags over one file are two writes against one drift lock.
    // Guarded so a refusal on one tag does not abandon the tags behind it.
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
    function renameTag(from, typed) {
      const to = foldTag(typed);
      shutEdit(TROW);
      if (!from || !to || to === from) { said(tagging, "unchanged"); return; }
      const over = carriers(from);
      fire(tagging, "rename-tag", over.map((r) => r.id), { from, to },
           `renamed :${from}:→:${to}:`).then(landing(to, (landed) => {
        // A row with both ends loses `from' and gains nothing — the server cuts.
        const gained = ttargets.filter((r) =>
          landed.has(r.id) && r.tags.indexOf(to) === -1).length;
        for (const r of ttargets)
          if (landed.has(r.id)) r.tags = renamedTags(r.tags, from, to);
        if (landed.size && tvocab.indexOf(to) === -1) tvocab.push(to);
        stepCount(to, gained);
        stepCount(from, -landed.size);
      }));
    }
    // The server's rule (`Glance.Query.renameTagEdits'): in place, deduplicated.
    const renamedTags = (tags, from, to) =>
      [...new Set(tags.map((t) => (t === from ? to : t)))];
    const TROW = {
      box: "tedit", pane: "tpane", fields: ["tname"],
      cells: ["title"], cols: TCOLS,
      mount: () => tmount,
      fill: (tag) => (el("tname").value = tag),
      focus: () => { el("tname").focus(); el("tname").select(); },
    };
    const renaming = () => { const e = editNow(); return !!e && e.o === TROW; };
    const openRename = () =>
      openOver(TROW, tagAt(), "org-rename-tag (no tag)");
    const cancelRename = () => cancelEdit("tag", TROW);
    const TFLAGS = {
      mount: () => tmount, at: tagAt, take: removeTags, note: unlogged,
      walk: () => stepIn(tmount, 1),
      missing: "this table-view.js has no delete flags",
      none: "org-toggle-tag (no tag)",
      unflag: "tag-unflag (flag cleared)",
      flag: "tag-flag (d again removes)",
    };

    // The binding the popup was RAISED by, which `o' inside it needs to echo
    // through.  An answer rather than the `let': a handle carrying a mutable
    // by value hands out whatever it held at boot.
    const openedBy = () => opening;
    // The mounts are made on first raise, so they leave as ANSWERS too: a
    // handle carrying a `let' by value hands out the `null' it held at boot,
    // and the popups' own `n'/`p' would step nothing forever.
    const linkMount = () => lmount;
    const tagMount = () => tmount;
    return { openedBy, linkMount, tagMount, addFlow, cancelLinkEdit, cancelRename, commitLink, landing, lediting,
             linking, managing, openLinkEdit, openRename, pointedLink,
             renameTag, renaming, showLinks, showPopup, showTags, shutLinks,
             shutTags, TFLAGS };
})({ CFG, again, askFrom, cancelEdit, echo, el, failed, fire, foldTag, listing,
     openEdit, remembered, rowsWord, said, selectedId, shortly, shutEdit,
     sole, soon, stepIn, tagFrom, unlogged,
     editNow: () => edit });
const { openedBy, linkMount, tagMount, addFlow, cancelLinkEdit, cancelRename, commitLink, landing, lediting,
        linking, managing, openLinkEdit, openRename, pointedLink,
        renameTag, renaming, showLinks, showPopup, showTags, shutLinks,
        shutTags, TFLAGS } = Popups;
