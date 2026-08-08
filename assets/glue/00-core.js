// The shell's script, and the whole of what the page does: boot, socket, key
// dispatch, sheets and palettes.  Vanilla JS with no framework, build step or
// dependency, and shrinking it beats adding to it (docs/invariants.md).
//
// THE BOOT is two /headlines fetches: 100 rows so the table paints without
// waiting on the store, then the rest behind it.  The full local set keeps
// movement, sorting and materialize coherent, and the renderer virtualizes.
// The socket then opens ?bootstrap=off.  A cold daemon answers 503 while it
// walks, and the boot renders that state and asks again.
//
// FILTERING IS THE SERVER'S: onFilter hands over the debounced query and the
// answer replaces the rows.  One fetch in flight at a time, a new one aborting
// the last.  Under a filter a row frame is answered by re-asking rather than
// splicing, only the server knowing whether the row still matches.  Every view
// swaps ON ITS ANSWER: the table up stands until the new rows are in hand.
// While the answer is out — or the socket gone — one class fades the table and
// every overlay, leaving them readable; the event strip and key line are exempt,
// being where a reader finds out why.
//
// The filter overlay is SUMMONED: `/' raises it through openFilter, the
// renderer's one entry point, and its lifecycle past that is the renderer's.
// The applied query is page state, written to the URL on every commit and
// restored through mount's initialQuery, so a filtered view is a link.  An
// EMPTY query is written as a `q' present and empty: absent means nobody has
// filtered and gets the default, present-and-empty a reader left alone.
//
// THE MATERIALIZE SHEET HAS NO BUTTONS: ESC or the backdrop flushes a dirty
// sheet and closes on the 200, C-x C-s flushes mid-edit, a 409 waits at
// `conflict', and a tab closing on a dirty sheet flushes with keepalive.  Two
// panes over one subtree, and the cut is the SERVER's — finding a drawer in org
// text is a parser's job and this page holds no parser.
//
// Keys that write without a sheet are POST /command: the page sends ids and a
// name, the server computes the spans, the rows come back over the socket, and
// the drift lock is the safety, so there is no confirmation step.  `D' takes
// the FLAGGED set and `t' the MARKED one — a mark is the generic bulk
// selection and a flag is made for archiving, so the destructive key inherits
// nothing.
//
// A LOST SOCKET COSTS ROWS and only `view-changed' costs the mount.  The
// reconnect asks /headlines for the applied query under the last tag: 304 keeps
// the rows, 200 replaces them, and either way the sheet, the palette, the
// selection and the URL stand.  Columns are what a row op cannot carry, so the
// reconnect also compares them — a daemon restarted while the page was away had
// no socket to say so.  Across a remount an unsaved sheet and a half-typed
// palette are stashed and restored.
//
// The keys are Glance.Web.Keymap's, parsed from a blob.  Row movement is the
// renderer's selectStep, which carries the column and crosses a page boundary
// this page is not told about.  The pill in the corner is the echo area.

    // The strip is an APPEND-ONLY event log; nothing clears it, so what a reader
    // missed is still there to scroll back to.  A line is `HH:MM:SS SEV scope
    // message' — the stamp muted, the severity coloured, the scope one word out
    // of a fixed set (ws, sync, cmd, filter, config, boot) naming which part of
    // the page is talking, each part a span so it carries its own colour, and
    // control characters in the message collapsed to spaces to keep it one line.
    // Past LOGCAP the OLDEST line is dropped, and a line identical to the one
    // before it bumps a counter — the only mutation an append-only strip allows,
    // and what keeps a retry loop from filling the ring with one message.  The
    // strip is capped in height, so keep its end in sight unless the reader has
    // scrolled up, which is a place they are holding on purpose.
    // One appended child: TAG under INTO, wearing CLS and holding TEXT when there
    // is any.  Both trees this page builds — the event strip's lines and the value
    // palette's entries — are rows of these.
    // Every server value in ONE blob, the keymap blob's own pattern:
    // eight per-build constants and the one per-request defaultQuery.
    const CFG = JSON.parse(document.getElementById("cfg").textContent);
    const part = (into, tag, cls, text) => {
      const e = document.createElement(tag);
      e.className = cls;
      if (text !== undefined) e.textContent = text;
      into.appendChild(e);
      return e;
    };
    const LOGCAP = 500;
    let logLast = null;
    function append(scope, sev, message) {
      const box = document.getElementById("log");
      const text = String(message).replace(/[\x00-\x1f]+/g, " ");
      const end = box.scrollTop + box.clientHeight >= box.scrollHeight - 4;
      if (logLast && logLast.scope === scope && logLast.sev === sev
          && logLast.text === text) {
        logLast.count.textContent = `×${(logLast.n += 1)}`;
      } else {
        const line = document.createElement("div");
        line.className = sev;
        part(line, "span", "lt", new Date().toTimeString().slice(0, 8));
        // The severity is SPELLED uppercase and WORN lowercase: the word is what
        // a reader scans a screenful of chatter for, the class is what the
        // stylesheet and the suite name, and the display is the only place one
        // value folds into two cases.
        part(line, "span", "lv", sev.toUpperCase());
        part(line, "span", "lc", scope);
        part(line, "span", "lm", text);
        logLast = { scope, sev, text, n: 1, count: part(line, "span", "ln", "") };
        box.appendChild(line);
        while (box.children.length > LOGCAP) box.removeChild(box.children[0]);
      }
      if (end) box.scrollTop = box.scrollHeight;
    }
    /**
     * WHERE THE KEYS ARE, as a control.  `active()' is an
     * `Element' and every reader here wants what a FORM control has — a value,
     * a selection, a blur — so the cast is spelled once rather than at each.
     * @returns {HTMLInputElement & HTMLTextAreaElement & HTMLElement}
     */
    const active = () => /** @type {any} */ (document.activeElement);
    /**
     * E's target, likewise: an `EventTarget' is what the DOM promises and a
     * control is what every listener on this page was handed.
     * @param {Event} e
     * @returns {HTMLInputElement & HTMLTextAreaElement & HTMLElement}
     */
    const targetOf = (e) => /** @type {any} */ (e.target);
    /**
     * The element ID names.  EVERY id this page reads is in the served markup,
     * so a miss is a page that was mis-built rather than a case to handle.
     *
     * Typed as the form-control intersection because most of what is looked up
     * here IS a control and the rest never read `value'.  What that costs is
     * element-KIND checking, which this page has never had; what it keeps is
     * every other check, the model shapes below among them.  The narrower
     * `document.getElementById' answer — `HTMLElement | null' — would put a
     * cast on ~90 call sites to say what the markup already says.
     * @param {string} id
     * @returns {HTMLInputElement & HTMLTextAreaElement & HTMLSelectElement}
     */
    const el = (id) =>
      /** @type {any} */ (document.getElementById(id));
    // THE WASH.  What is on screen stops being known to be current in exactly
    // two ways: the view is being replaced and its answer has not landed, or
    // the socket that would deliver a change is gone.  A reader cannot tell
    // either from a page that is simply quiet, so both wear ONE look — faded
    // back, never blurred, stale rows being still the rows and having to stay
    // readable while the answer is on its way — carried by ONE class on the
    // document element.  The event strip and the key line are exempt, being
    // where a reader finds out why.
    //
    // Each reason arms on a DELAY, which is the whole of what keeps the wash
    // off a page that is working: a fetch answering inside its grace and a
    // socket that blips and comes back dim nothing at all.  Whoever arms a
    // reason is who clears it.
    const WASH = { view: 300, socket: 400 };
    const wash = {
      n: { view: 0, socket: 0 }, at: { view: 0, socket: 0 },
      on: { view: false, socket: false },
      // Reason WHY now stands COUNT times over: one arming discipline for both,
      // differing in who counts.  A view fetch STEPS the count, `load'
      // overlapping an abort with the fetch that replaced it where a boolean
      // would clear the wash the replacement still wants; the socket SETS it,
      // a connection closing before it ever opened arming twice on one open.
      want(why, count) {
        const was = this.n[why];
        this.n[why] = Math.max(0, count);
        if (this.n[why] === was) return;
        if (this.n[why]) this.arm(why); else this.off(why);
      },
      step(why, by) { this.want(why, this.n[why] + by); },
      arm(why) {
        if (this.on[why] || this.at[why]) return;
        this.at[why] = setTimeout(() => {
          this.at[why] = 0; this.on[why] = true; this.show();
        }, WASH[why]);
      },
      off(why) {
        clearTimeout(this.at[why]); this.at[why] = 0;
        this.on[why] = false; this.show();
      },
      show() {
        document.documentElement.classList.toggle("stale",
          this.on.view || this.on.socket);
      },
    };
    // Does MOUNT carry the optional call NAME?  Every renderer capability this
    // page uses is detected before it is used, and there are TWO mounts now —
    // the table and the sheet's property panel — so the question is asked of a
    // handle rather than of the one this page used to have.
    const can = (mount, name) => !!mount && typeof mount[name] === "function";
    // ROW MOVEMENT, ONCE.  Both spellings and the arrows, as a direction or
    // zero for a key that is not one — read by the MODAL mounts, whose keys live
    // in private listeners around the dispatch (the property panel's registers
    // ahead of it, the rest behind).  Written once, so a third spelling is one
    // edit and the map's own `n'/`p'/`j'/`k' rows cannot drift from what a modal
    // surface answers to.  The table's own movement stays the map's, walking two
    // axes and pages.
    const rowStep = (k) => (k === "<down>" || k === "n" || k === "j" ? 1
                          : k === "<up>" || k === "p" || k === "k" ? -1 : 0);
    const stepIn = (mount, step) =>
      can(mount, "selectStep") && mount.selectStep(step);
    // The archive/delete flags, which are the one capability both mounts want:
    // the table flags a row for archiving and the panel flags one for deleting,
    // and an asset predating either says so once.
    const flagsOn = (mount) => can(mount, "flagRow") && can(mount, "getFlagged");
    // WHERE A MOUNT'S CURSOR IS, as an id.  The renderer's own answer, asked for
    // at the moment it matters and never kept here, and GUARDED: a mount with
    // nothing selected answers with nothing, three surfaces read it, and an
    // unguarded deref is a throw over an empty popup.
    const selectedId = (mount) =>
      (can(mount, "getSelection") ? (mount.getSelection() || {}).id : null) || null;
    // On the next frame, or now where there are no frames.  What the panel's
    // edit overlay waits for: the renderer stamps its selection in a frame of
    // its own, so a row selected in this tick has no marked element yet.
    const soon = (fn) =>
      (typeof requestAnimationFrame === "function" ? requestAnimationFrame(fn)
                                                    : setTimeout(fn, 0));
    let table = null, socket = null, backoff = 1000, editing = null;
    // The sheet's own baselines: the textarea as the file holds it as far
    // as this page knows, and the panel's drawer likewise.  The structured
    // DOCUMENT keeps none, every element in it committing on its own.  Where
    // the sheet STANDS is `subtreeSheet.state'.
    let base = "", baseProps = null, raw = false;
    // The server filters and pages; these hold the query it was last asked
    // with, the fetch still in flight for it, and the timer that re-asks
    // when a row frame lands while one is on.
    let query = "", inflight = null, requeryAt = 0;
    // WHERE POINT GOES WHEN AN ARCHIVE TAKES ITS ROW AWAY.  Armed at fire time,
    // the last moment the view still holds the rows about to leave, and spent
    // when they have left.  `from' is the row point was standing on, `id' the
    // row to land on and `at' its place among the survivors; null whenever point
    // was NOT on a leaving row, since nothing is owed then.
    let leaving = null;
    // AND WHERE POINT GOES WHEN A CAPTURE MAKES ONE.  The mirror of `leaving',
    // and the other half of one rule: a write that moves the view says where
    // point is owed and the arriving rows spend it.  A capture is the one write
    // that MAKES a row, and the id is the answer's — a minted `ORG_GLANCE_ID' for
    // a blob, the target file's ordinal for an inbox line.
    let arriving = null;
    // The tag the last answer carried, which is what makes a reconnect
    // cheap: an unmoved store answers the revalidation 304 and no rows
    // cross the wire at all.
    let etag = null;
    // One number, two jobs: the boot asks for this many rows and the renderer
    // shows this many at a time, so the first paint is exactly page one and
    // the set arriving behind it only adds pages to turn to.
    const PAGE = 100;   // rows in the first paint, and rows to a page
    function mount(view) {
      // The trail comes off the URL before the mount, because `chipLabel' can
      // be asked for a label during the first paint: the map has to be standing
      // when the renderer draws the chip it aliases.
      const was = bootTrail();
      crumbLabels = was.labels;
      crumbSels = was.sels;
      table = TableView.mount(document.getElementById("app"), view, {
        palette: true,     // the filter is summoned, never resident
        // The set is shown a page at a time: the renderer keeps the window,
        // the spacers and the pager in its own status line, and movement
        // crosses the boundary without this page knowing where one is.
        pageSize: PAGE,
        // Marking is the renderer's chrome and the renderer's state: a
        // checkbox column it draws and a set of ids it keys, which is why a
        // mark outlives a filter that hides its row and a page it is not on.
        // This page owns the keys and nothing else.
        marks: true,       // dired's m/u/U/M, drawn and counted by the renderer
        // A flagged row's hint is the two keys that answer the flag, spelled the
        // way the key line spells them.  The renderer draws it; an asset
        // predating the option drops it the way it drops any other it has no
        // field for.
        flagHelp: "d/D archive · u unflag",
        // The renderer's per-row hint says RET materializes, which the key line
        // under the table already says and says for every command.  One place.
        actionHints: false,
        // The applied query, restored as the renderer's own committed
        // chips. It tokenizes them and delivers nothing — the rows in
        // hand are already the server's answer to this query, and a
        // delivery here would ask for them a second time.
        initialQuery: query,
        // A `ref:' chip shows what the drill was FOR, never the row id it
        // is spelled with. The query is untouched — the renderer aliases
        // the display alone — so DEL still strips the token as written.
        chipLabel: (tok) => crumbLabels[tok] || null,
        onAction: (command, id) =>
          command === "materialize" ? materialize(id)
                                     : append("cmd", "info", `action: ${command}  id=${id}`),
        onLink: (target) => append("cmd", "info", `link: ${target}`),
        onFilter: filter,   // the server narrows; the renderer shows what it is given
        // The pin button-badge at the chip strip's far edge: the renderer
        // reports the click and wears the boolean, this page decides both —
        // `P''s own write, and `pinnedQuery' as the truth the badge compares
        // against.  An asset without the option draws nothing and `P' still
        // works, which is the key staying the spine and the button the touch
        // door.
        onPin: () => pinHere(),
        pinned: query.trim() === pinnedQuery,
      });
      mainCols = (view || {}).columns || [];
      // An asset older than `initialQuery' drops it silently, which would
      // leave the page showing no filter over rows that are filtered.
      // `getQuery()' says whether it took: when it did not, put the query
      // back in the box the way this did before chips could carry it.
      if (query && !holds(query)) showQuery();
      // The strip goes back up the way the query did.  `setCrumbs' keeps only
      // what parses as a crumb, so a hand-edited parameter costs the trail and
      // nothing else.  An asset with no crumbs draws none and the labels sit
      // unread until one arrives — a drill is refused before it starts.
      if (crumbing() && was.trail.length) table.setCrumbs(was.trail);
      // The columns are the view's: both halves of a filter read the keys
      // out of them (`parity'), and cell movement names its landing column
      // by the header sitting over it.
      cols = view.columns || [];
      // Whatever the remount that led here took down goes back up over the
      // new table; on a first boot there is nothing stashed and nothing to do.
      restore();
    }
    // One /headlines at a time: a keystroke aborts the fetch before it, so
    // an earlier answer can never land over a later one.  TAG makes it a
    // revalidation: the browser's own cache is stepped around, so the tag
    // that goes out is this page's and the 304 comes back as the answer it
    // is rather than as a body the cache filled in behind it.
    function load(params, tag) {
      if (inflight) inflight.abort();
      inflight = new AbortController();
      const init = { signal: inflight.signal };
      if (tag) { init.headers = { "if-none-match": tag }; init.cache = "no-store"; }
      return fetch(`/headlines${params}`, init).then((r) =>
        // 304: the store has not moved, so there is no view to read and the
        // rows already on screen are the current answer to this query.
        r.status === 304 ? { view: null, total: 0 }
        : r.ok ? r.json().then((view) => {
            etag = r.headers.get("ETag") || etag;
            return { view, total: +r.headers.get("X-Glance-Total") };
          })
        // 503 is the startup walk: the server is listening and says so
        // in the body.  `start' polls it; nothing else can see it.
        : r.status === 503 ? r.json().then((b) => { throw Object.assign(new Error("indexing"), { indexing: b }); })
             : r.text().then((t) => { throw new Error(t); }));
    }
    const quiet = (e) => {
      if (e.name !== "AbortError") append("ws", "error", `load failed: ${e.message}`);
    };
    // A fetch whose answer REPLACES what is on screen, marked as one: it holds
    // the wash's view reason while it is out, so a swap slower than the grace
    // says so rather than leaving stale rows looking current.  The parity
    // baseline and the probe behind `@' go through `load' without this, neither
    // replacing anything and dimming a page for a fetch that will not change it
    // being the same lie the other way round.  A boot holds nothing either: a
    // page with no table on it has no stale content to wash.
    const viewing = (p) => {
      if (!table) return p;
      wash.step("view", 1);
      return p.finally(() => wash.step("view", -1));
    };
    // The unfiltered answer is kept: with a filter on, the loaded rows are
    // the server's answer to it and cannot be used to check that answer.
    let all = [], cols = [];
    const paint = (a) => {
      const rows = a.view.rows || [];
      table.setRows(rows);
      if (!query) all = rows;
      parity(a.total);
    };
    // The check needs an unfiltered set to check a filtered answer against,
    // and this page can open filtered — a `?q=' link, or the default view
    // below.  A paint under a query arms nothing, so a filtered session
    // would keep the check dark for as long as it lasted.  Ask for the
    // unfiltered set once, behind everything else, keep it as the baseline
    // without touching the table, and re-run the check that had nothing to
    // run against when TOTAL was painted.
    function arm(total) {
      if (!query || all.length) return;
      load("").then((a) => { all = a.view.rows || []; parity(total); }).catch(quiet);
    }
    // A suggestion must never silently offer what the applied path cannot
    // evaluate.  The keys that can differ between the two halves are the
    // producer's virtual ones — the columns are in the view both read — so
    // when the server answers a query carrying one with nothing at all and
    // the words are in the rows this page already holds, say so.  Loose and
    // one-directional on purpose: it reports a suspicion and corrects
    // nothing, since guessing which half is right is how they drift.
    function parity(total) {
      if (total !== 0 || !query || !all.length) return;
      if (typeof TableView.parseQuery !== "function") return;
      const keys = cols.map((c) => c.key);
      const loose = TableView.parseQuery(query, keys).filter((t) =>
        t.key === null && !t.quoted && !t.negated && /^[^:=]+[:=]./.test(t.value));
      if (!loose.length) return;
      const wants = loose.map((t) => t.value.slice(t.value.search(/[:=]/) + 1).toLowerCase());
      const text = (r) => keys.map((k) => TableView.displayText((r.cells || {})[k]))
        .join("\x1f").toLowerCase();
      const local = all.filter((r) => wants.every((v) => text(r).includes(v))).length;
      if (!local) return;
      const note = "filter parity divergence — asset/daemon version skew";
      console.warn(note, { query, server: total, local });
      append("filter", "warn", note);
      echo(note);
    }

    // The applied query is page state.  It rides in the URL, so a filtered
    // view is a link and a reconnect comes back to it; DEL takes its last
    // token off through the renderer.  The shell sends the string as typed
    // — the grammar is the server's to parse (SCHEMA.md).
    const params = () => new URLSearchParams(location.search);
    const urlQuery = () => params().get("q") || "";
    // What the page opens on when the address bar says nothing, and what
    // `g' applies.  The daemon embeds it at request time out of the tree's
    // own `#+GLANCE_DEFAULT_FILTER:', falling back to org-glance's spelling
    // of the active group.  A `?q=' is the user's intent whatever it holds,
    // an empty one included, so the default is injected only where there is
    // no `q' at all — and then it is a query like any other, committed to
    // the URL, shown as the renderer's chip and asked of the server.
    // THE TREE'S SAVED VIEWS: the boot's copy of what `/config' serves, in the
    // registry's own order, each `{id, query}'.
    const seedView = (id) =>
      ((CFG.views.find((v) => v.id === id) || {}).query || "").trim();
    const DEFAULT_QUERY = seedView("default");
    // What the pin badge compares against, and what `a' applies.  The
    // page-embedded values seed them; a successful write moves them, since the
    // blob describes the BOOT's config and the write has just changed the tree's.
    let pinnedQuery = DEFAULT_QUERY.trim();
    let agendaQuery = seedView("agenda");
    // The settings sheet's view COMPOSER: the same table-view filter bar the
    // main page carries, mounted once over `#cfbox' with no table behind it,
    // showing whichever view `#cwhich' names.  `vrows' is every view with its
    // own text, the box being a VIEW of `vrows[vat]' — the layer boxes' rule, so
    // switching costs no request and loses no edit.
    /**
     * ONE SAVED VIEW as this sheet holds it: the id the wire spells, the query
     * as served, and the query as the composer holds it now.
     * @typedef {object} ViewRow
     * @property {string} id
     * @property {string} base
     * @property {string} text
     */
    /** @type {ViewRow[]} */
    let vrows = [];
    let cmpose = null, mainCols = [], vat = "default";
    const vrow = () => vrows.find((v) => v.id === vat);
    const composerQuery = () =>
      (cmpose && can(cmpose, "getQuery") ? cmpose.getQuery().trim()
       : vrow() ? vrow().text : "");
    const bootQuery = () => (params().has("q") ? urlQuery() : DEFAULT_QUERY);
    // The drill-down trail.  The STACK is the renderer's — it draws the crumbs,
    // and `setView' drops them with the world they described — so this page
    // keeps no copy of it and reads it back when it needs one, the way it keeps
    // no copy of the marks or of the selected column.  What it does keep is the
    // LABEL a `ref:' token wears, since no lookup can recover it: the title
    // belongs to the row referred TO, which is very rarely among its own
    // referrers, so by the time the drill has landed the title is nowhere in the
    // rows on screen.  Keyed by the token, so one map answers both readers —
    // `chipLabel' aliasing the live chip, and the crumb a further drill leaves.
    let crumbLabels = {};
    const crumbing = () => can(table, "pushCrumb") && can(table, "popCrumb")
      && can(table, "getCrumbs") && can(table, "setCrumbs");
    const trail = () => (crumbing() ? table.getCrumbs() : []);
    // The selection each crumb was pushed FROM, one entry per crumb.  It rides
    // BESIDE the trail rather than inside it, the renderer's `crumbOf' keeping a
    // crumb's `label' and `query' and dropping everything else, so a selection
    // put in a crumb would never come back out of `getCrumbs()'.  The renderer's
    // DEPTH is still the truth: a side table fallen out of step with it is
    // dropped whole rather than pairing a crumb with another crumb's row.
    let crumbSels = [];
    const selsFit = () => crumbSels.length === trail().length;
    // Where a landing puts the cursor.  ONE function, three rules differing only
    // in what they ask for: an APPLIED view — a palette commit, `g', `a', `@' —
    // asks for nothing and takes the FIRST row of the answer, a POP asks for the
    // row its drill was launched from, and an ARCHIVE asks for the row after the
    // ones it took away, at the place they left.  An empty answer selects
    // nothing, whichever asked.  `select' answers false for a row the view no
    // longer holds, so a remembered row an edit or a narrower filter took away
    // falls through to AT — index 0 for the two callers that name none, the
    // first-row landing spelled as the general rule rather than beside it.  The
    // COLUMN rides across either landing: a commit repaints the same mount, so
    // the cell the reader was reading in is still there to land in, and `^',
    // which is a commit now, would otherwise take the selection it needs away
    // from the next press of itself.  After a REMOUNT there is no column to keep
    // and `column()' answers null, the whole-row look this landed on before.
    function land(sel, back) {
      if (!can(table, "select")) return;
      const rows = visible();
      if (!rows.length) return;
      if (sel && sel.id
          && table.select(sel.id, sel.col === null ? undefined : sel.col)) return;
      const at = column();
      const i = Math.max(0, Math.min(back || 0, rows.length - 1));
      table.select(rows[i].id, at === null ? undefined : at);
    }
    // A row as the `ref:' token naming it.  The value is quoted where the id
    // carries a token separator: the fallback row id is `PATH#K' and a path may
    // hold a space, which the grammar would otherwise cut the token at.  An id
    // carrying a QUOTE is beyond this — the scanner drops quote characters
    // rather than unescaping them — and no id spelling seen in the corpus does.
    const refToken = (id) => `ref:${/[\s&"]/.test(id) ? `"${id}"` : id}`;
    // What the view being LEFT is called, for the crumb that stands in for it.
    // A labelled jump chains honestly: drilling out of a drill leaves the first
    // drill's own name behind rather than its `ref:' spelling, and any other
    // query is its own best name.
    const hereLabel = () => crumbLabels[query] || query || "all rows";
    // Every applied query is written, the EMPTY one included: a `q' that is
    // present and empty is a reader who took the filter off, where an absent
    // one is a page nobody has filtered yet.  Only the second has the default
    // injected over it, so DEL'ing the last chip survives a reload and every
    // remount after it — deleting the parameter here is what made a cleared
    // view come back filtered.
    //
    // The trail rides beside it, and the URL is the ONLY channel it crosses
    // a remount by: every mutation of the stack — a drill, a pop, `g' — is
    // followed by a `remember', so the address bar is current whenever
    // `mount' reads it back.  That is why `stash'/`restore' say nothing
    // about crumbs: what they carry is work the reader has NOT committed,
    // and there is no such thing as a half-applied crumb.
    function remember(q) {
      const p = params();
      p.set("q", q);   // `keys' and anything else in the URL survives
      const t = trail(), labels = Object.keys(crumbLabels).length ? crumbLabels : null;
      if (!t.length && !labels) p.delete("crumbs");
      else p.set("crumbs", JSON.stringify( { trail: t, labels: crumbLabels, sels: selsFit() ? crumbSels : [] }));
      history.replaceState(null, "", `?${p.toString()}`);
      // Every applied query passes through here, so here is where the badge
      // learns whether the view on show IS the pinned one.
      if (can(table, "setPinned")) table.setPinned(q.trim() === pinnedQuery);
    }
    // The trail as the address bar carries it.  A parameter a hand has been in
    // is not worth a diagnostic: anything that does not parse into the two
    // fields is one boot without a trail, which is where a reader starts anyway.
    function bootTrail() {
      try {
        const was = JSON.parse(params().get("crumbs") || "null");
        if (!was || typeof was !== "object")
          return { trail: [], labels: {}, sels: [] };
        return {
          trail: Array.isArray(was.trail) ? was.trail : [],
          labels: was.labels && typeof was.labels === "object" ? was.labels : {},
          sels: Array.isArray(was.sels) ? was.sels : [],
        };
      } catch (e) { return { trail: [], labels: {}, sels: [] }; }
    }
    // A query as the `/headlines' query string asking it, spelled once for
    // the four callers that want it — the boot, a commit, the arming fetch
    // and the reconnect.  A second spelling is how a revalidation comes to
    // be answered 304 against rows answering some other question.
    const asking = (q) => (q ? `?q=${encodeURIComponent(q)}` : "");
    // One place asks the server for rows: `query' is already what to ask.
    // A COMMIT is a new question, so the cursor has no claim on the answer: it
    // REPAINTS rather than remounting and would otherwise stay wherever it was,
    // on a row the new answer may not hold, so it takes the same first-row
    // landing `applyView' gives every applied view.  A REFETCH THE WATCH CAUSED
    // is the view the reader already had, arriving again because a file moved,
    // so it lands nothing of its own: the renderer keeps the cursor where it
    // was — on its row while that row is still there, else at the same visual
    // place — and only the archive that took the rows away may override that,
    // saying so by arming `leaving'.  That carve is what stops somebody else's
    // edit yanking a reader back to row one.
    const fetchRows = (landing) =>
      viewing(load(asking(query)))
        .then((a) => { if (!table) return;
                       // The COLUMNS moved — a `columns:' token committed or
                       // taken off — and a paint cannot reshape a mount, so
                       // this is the reconnect's own comparison at the commit
                       // door: the view goes back up whole, columns and all.
                       if (a.view && !sameColumns(a.view.columns || []))
                         { remount(landing && (() => landing())); return; }
                       paint(a);
                       if (landing) landing(); else land(null); })
        .catch(quiet);
    // A commit is the moment a NEW query goes to the server — a settled
    // debounce, a committed token, an accepted completion.
    function commit(q) {
      if (q === query) return;
      query = q;
      leaving = arriving = null;   // both belonged to the view being left
      remember(q);
      fetchRows();
    }
    const filter = (q) => commit(q.trim());
    // The query's last token comes off in the renderer, which owns the
    // chips showing it: a shell-side strip would leave them on screen
    // spelling a filter that is no longer applied.  An asset too old to
    // have the pair says so rather than growing a second implementation.
    const strips = () => can(table, "stripLastToken") && can(table, "getQuery");
    // Whether the mounted renderer is carrying Q as its own query.
    const holds = (q) => can(table, "getQuery") && table.getQuery() === q;
    // The renderer's filter field, wherever its mode puts it: the palette's
    // input in palette mode, the resident box in an asset predating one.
    // Named once, since three callers want it and none of them may reach
    // further into the chrome than this.
    /**
     * The renderer's own resident filter box, when the asset draws one.  Cast
     * for `el''s reason: a `querySelector' answers `Element' and every reader
     * here wants what a control has.
     * @returns {(HTMLInputElement & HTMLElement) | null}
     */
    const filterBox = () =>
      /** @type {any} */ (document.querySelector("#app .tv-filter"));
    // The fallback for an asset without `initialQuery': the query goes in
    // the box rather than into chips.  The box is the renderer's, and
    // setting its value fires no input event, so a restored query shown
    // there is not committed a second time.
    function showQuery() {
      const box = filterBox();
      if (box) box.value = query;
    }

    // An answer unwrapped, with the server's own error thrown: the routes
    // that read a value want one handling of a refusal, so the shape sits
    // here once and both doors below take it.
    const unwrap = (r) => r.json().then((b) => {
      if (!r.ok) throw new Error(b.error || r.status);
      return b;
    });
    const getJSON = (url) => fetch(url).then(unwrap);
    // And a JSON POST: the method, the one header and the encoding decided
    // once, for every route that takes a body.  EXTRA is what a page closing
    // on an edited sheet adds — `keepalive', being the one caller that
    // cannot wait.
    const postJSON = (url, body, extra) =>
      fetch(url, {
        method: "POST",
        headers: { "content-type": "application/json" },
        body: JSON.stringify(body),
        ...extra,
      });
    // What a WRITE answers, status and body together: a 409 carries a body
    // saying which kind it is, so both are read rather than the status alone.
    const outcome = (r) => r.json().then((b) => ({ status: r.status, body: b }));

    // The two shapes of /headline, each written once.  `post' pins
    // the write to DIGEST.
    // The route's own address, and the ONE place this page spells it: a row id,
    // and the index of an entry inside that row's subtree where the sheet has
    // walked into one.  A child is a number the SERVER handed over — every
    // answer names the entries under it and the one above it — so this page
    // counts no stars and holds no outline of its own.
    const at = (id, child) => `/headline?id=${encodeURIComponent(id)}`
      + (child === null || child === undefined ? "" : `&child=${child}`);
    const headline = (id, child) => getJSON(at(id, child));
    const post = (id, digest, asked, extra, child) =>
      postJSON(at(id, child), { ...asked, digest }, extra);
    function materialize(id) {
      headline(id).then((h) => show(h, false))
        .catch((e) => append("sync", "error", `materialize failed: ${e.message}`));
    }
    // ONE PANE over one subtree, and `raw' says which one is showing.  The
    // structured document is the resident shape and commits per element; the
    // textarea is the escape hatch, and it keeps the buttonless ladder —
    // `base' is what the file holds as far as this page knows, and `dirty()'
    // over it is the whole of what decides whether closing costs a POST.
    function show(h, asRaw) {
      editing = h; raw = !!asRaw;
      el("mfile").textContent = `${h.file}  ·  ${h.id}`;
      fill(h);
      sync("synced");
      el("modal").className = "on";
      // Raw mode is a textarea and takes the focus; the document holds the keys
      // with NOTHING focused, the way the panel's nav did — which is what leaves
      // every printable key free to be movement and a command.
      if (raw) el("mtext").focus(); else el("mtext").blur();
    }
    // Both panes filled from H.  The document keeps NO baseline — every
    // element in it commits on its own — so what `dirty()' is measured
    // against is the panel's own model and, in raw mode, the textarea.
    function fill(h) {
      base = raw ? h.org : "";
      el("mtext").value = base;
      // TOGGLE, never assign.  The sheet's class carries its SIZE TIER as well
      // as its shape, and a wholesale write drops the tier on the first
      // materialize — silently, since the markup still reads right and only a
      // live page is a size.  `classList' spells "set one class, keep the rest",
      // keeping the tier a fact of the element rather than a string to respell.
      el("sheet").classList.toggle("raw", raw);
      shutEdit(DTITLE); shutEdit(DPARA);
      dflags.clear();
      // THE LINKS COME WITH THE MATERIALIZE: the answer carries the row's
      // whole scan beside the text it describes, so the display is compact
      // from the first frame and there is no async gap to bridge — the raw
      // flash a second request opened, and the keep-stale guard that bridged
      // it, both retired by the one-answer design.
      dlinks = h.links || [];
      if (raw) { drows = []; dlines = []; drawDoc(); } else docFrom(h);
      drawProps(raw ? [] : h.properties || [], raw ? [] : h.planning || []);
      el("mdoc").className = raw ? "" : "on";
      drawWhere(h.path || []);
      drawLog(raw ? "" : h.logbook || "");
      baseProps = raw ? null : edited();
    }
    // Everything the panel holds, as one string to compare against.  Two lists
    // rather than one, so a property and a planning entry spelling the same pair
    // cannot cancel out.
    const edited = () => JSON.stringify([props(), planning()]);
    // THE SHEET'S OWN CRUMB STRIP, the drill stack's rhyme one level in: the
    // table leaves a crumb when `@' drills into a reference, the sheet leaves one
    // when `RET' drills into a child, and `DEL' walks both back, so both draw the
    // same thing.  STANDING, so it is a place rather than a notification — the
    // ROW alone is one crumb and each descent appends, where a strip appearing on
    // the way down would move the panes under the reader as they arrived.
    //
    // It wears the renderer's own MUTED CHIP — same silhouette, dimmed ink:
    // `.tv-chip' plus `.tv-chip-muted', hand-copied the way `--g-border' is,
    // those rules living inside `.tv-root' where nothing outside a mount reaches
    // them.  The LAST crumb is where the reader stands and takes the full ink,
    // the one thing the strip says that the renderer's own crumbs do not have to.
    // Inert: `DEL' is the climb and the key line teaches it, so there is nothing
    // to click and no hint crowding the bar.
    function drawWhere(path) {
      const bar = el("mwhere");
      bar.textContent = "";
      path.forEach((title, i) =>
        part(bar, "span", "wc" + (i === path.length - 1 ? " wat" : ""),
             title || "(untitled)"));
    }
    // The logbook strip: shown, never sent, and taken off the sheet outright
    // when there is none rather than left as a labelled blank.  The drawer's
    // INTERIOR alone — `:LOGBOOK:' and `:END:' delimit the thing the widget
    // already is, so showing them spends two of the strip's lines saying what the
    // strip is.  The cut is display-only: what goes back into the file is the
    // whole drawer, delimiters and all, and this page never sends it at all.
    function drawLog(text) {
      const inner = text.replace(/\n$/, "").split("\n").slice(1, -1).join("\n");
      el("mlog").textContent = inner;
      el("mlog").className = inner ? "on" : "";
    }
    // DIRTY IS THE PANEL'S AND RAW MODE'S.  The structured document commits per
    // ELEMENT — each write its own drift-locked splice, each answer re-pinning
    // the digest — so it never holds work nobody wrote; the panel's model and the
    // textarea are the two that can, keeping the whole ladder they always had:
    // flush on the way out, `conflict' and `error' waiting for a keystroke.
    const dirty = () => editing !== null
      && (raw ? el("mtext").value !== base : edited() !== baseProps);
    // THE STRUCTURED DOCUMENT, the sheet's LEFT pane.  A subtree's TEXT is a
    // HEADLINE LINE with cells, body paragraphs and the children under it, drawn
    // in file order for the cursor to walk; the drawer and the planning line
    // stay the PANEL's, being a list of records.
    //
    // FLOWING TEXT until the cursor lands: nothing is boxed, ruled or labelled
    // while it is being read, and the ELEMENT under point wears the page's own
    // selection exactly as a table row does.
    //
    // NO TABLE-VIEW MOUNT, the one place on the page with none: the renderer's
    // widget draws a list of RECORDS, one shape per row, where this is a list of
    // KINDS sharing no columns.
    //
    // MODEL AND VIEW.  `drows' is the model, `drawDoc' the whole view.  A commit
    // moves the model, so an open edit is not a change and cannot be written.
    //
    // PER-ELEMENT COMMITS: every element writes on its own — a lens splice for a
    // paragraph, a `/command' for the headline's cells — each under the file's
    // digest and each re-pinning it from the answer.  So nothing here is ever
    // unsaved, and the sheet's dirty ladder is the PANEL's alone.
    const DCELLS = CFG.dcells;
    // The model, the cursor and the body's own lines.  GRAIN is reserved: the
    // cursor covers one ELEMENT today and the field is what a future
    // expand-region moves — a paragraph's line, a subtree, the whole document —
    // without every reader of the cursor learning about it twice.
    let drows = [], dat = 0, dcol = null, dgrain = "element";
    // id -> immediate owner id, the ladder the ancestor tests climb.
    let dparent = {};
    const downersOf = (id) => {
      const chain = [];
      for (let o = dparent[id]; o; o = dparent[o]) chain.push(o);
      return chain;
    };
    let dlines = [];
    // The ELEMENT the draw put the cursor on, kept so the edit overlay can be
    // anchored to it.  The `dat'-th child of `#dlist' is NOT that element: a
    // composite draws its leaves INSIDE it, so the two stop agreeing at the
    // first list or block in the document and every edit below one anchored to
    // the wrong element.  The draw is what knows which box it marked.
    let dcursor = null;
    // The flags are the document's own, keyed by element id the way the
    // renderer's are keyed by row id — a Set and four calls, which is exactly
    // what `flagKey' feature-detects, so the deletion gesture is the page's one
    // implementation over a fourth surface.
    const dflags = new Set();
    const dmount = {
      // Each of the three that MOVES a flag redraws, since the wash is the
      // draw's: a mount would have repainted itself and this widget is the
      // page's own, so the redraw is where the set is written.
      flagRow: (id) => { dflags.add(id); drawDoc(); },
      unflagRow: (id) => { dflags.delete(id); drawDoc(); },
      getFlagged: () => [...dflags],
      clearFlags: () => { dflags.clear(); drawDoc(); },
    };
    // H's four cells, in the order org writes them on a headline line.  One
    // reading for the headline itself and for every child, since a child line IS
    // a headline line drawn one level in.
    const cellsOf = (o) => DCELLS.map((k) => ({ key: k, val: (o || {})[k] || "" }));
