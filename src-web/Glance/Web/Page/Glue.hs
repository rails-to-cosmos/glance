-- | The shell's inline script, and the whole of what the page does: boot,
-- socket, key dispatch, sheets and palettes.
--
-- Inline so the shell has exactly one asset to find.  Vanilla JS with no
-- framework, build step or dependency, and shrinking it beats adding to it
-- (docs\/invariants.md).
module Glance.Web.Page.Glue (shellGlue) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)

import qualified Data.Text as T

import Glance.Query (captureCodes, followableTypes, linkColumns, planningKeywords, tagColumns)
import Glance.Web.Base (jsonValue, logLinesDefault, logLinesMax, logLinesMin)

-- | The shell's @\<script\>@ element, WANTED being the tree's default view.
--
-- The boot is two @\/headlines@ fetches: 100 rows so the table paints without
-- waiting on the store, then the rest behind it.  The full local set keeps
-- @n@, @p@, sorting and materialize coherent, and the renderer virtualizes, so
-- 13k rows cost memory and no DOM.  The socket then opens @?bootstrap=off@,
-- the rows being already here.  A cold daemon answers the first fetch 503 while
-- it walks, and the boot renders that state — amber dot, @indexing …@ with the
-- body's elapsed seconds — and asks again a second later.
--
-- Filtering is the server's: @onFilter@ hands over the debounced query, the
-- shell asks @\/headlines?q=@ for it exactly as typed ('Glance.Web.Filter' owns
-- the grammar), and the answer replaces the rows.  One fetch is in flight at a
-- time and a new one aborts the last.  Under a filter a row frame off the socket
-- is answered by re-asking rather than by splicing, only the server knowing
-- whether the changed row still matches.  Every view swaps ON ITS ANSWER: the
-- table up stands until the new rows are in hand, then goes in one mount, and a
-- re-application asks for the whole answer rather than a page.  While the answer
-- is out — or the socket gone — one class on the document element fades the
-- table and every overlay over it, leaving them readable; each half arms on a
-- delay, and the event strip and key line are exempt, being where a reader finds
-- out why.
--
-- The filter overlay is summoned: the mount asks for @palette@, @\/@ raises it
-- through @openFilter@, the renderer's one entry point, and its lifecycle past
-- that is the renderer's, so this page never reaches into that chrome.  A
-- coarse pointer has no @\/@ to press — the one exception the keyboard-first
-- rule makes — so the chip row is 44px of tap target, labelled while empty,
-- summoning the same palette.  The applied query is page state, written to the
-- URL on every commit (@replaceState@, leaving @keys@) and restored through
-- @mount@'s @initialQuery@, so a filtered view is a link and a reconnect comes
-- back to it.  An EMPTY query is written as a @q@ present and empty: absent
-- means nobody has filtered this page and gets the default, present-and-empty a
-- reader who took the filter off and is left alone.  @DEL@ over the table is
-- that query's backspace — the last token, quotes and all.
--
-- The materialize sheet has no buttons: @ESC@ or the backdrop flushes a dirty
-- sheet and closes on the 200, a pristine one closes with no request, @C-x
-- C-s@ flushes mid-edit and chains the receipt's digest, a 409 keeps it open at
-- @conflict@ where @C-x C-s@ re-reads the digest and overwrites and @ESC@
-- discards, and a tab closing on a dirty sheet flushes with @keepalive@.  Two
-- panes over one subtree — textarea for the body, panel for the drawer, a row
-- of two fields per property with an empty row at the bottom that grows the
-- next — and the cut is the SERVER's, finding a drawer in org text being a
-- parser's job and this page holding no parser.  @C-c '@ swaps two-pane and raw
-- by re-materializing, refusing a dirty sheet.
--
-- Two keys write without a sheet: @D@ archives the FLAGGED set, else the row at
-- point, and @C-c C-t@ sets a state over the MARKED set, else the row at point.
-- A mark is the generic bulk selection and a flag is made for archiving, so the
-- destructive-looking key inherits nothing.  Both are @POST \/command@
-- ('Glance.Web.Commands.runCommand'): the page sends ids and a name, the server
-- computes the spans (@edit-link@ excepted, its range having come out of @GET
-- \/links@), the rows come back over the socket, and the drift lock is the
-- safety, so there is no confirmation step.
--
-- A lost socket costs rows and only @view-changed@ costs the mount.  The
-- reconnect asks @\/headlines@ for the applied query under the last tag: 304
-- keeps the rows, 200 replaces them in place, and either way the sheet, the
-- palette, the selection and the URL stand.  Columns are what a row op cannot
-- carry, so @view-changed@ remounts — and a daemon restarted while the page was
-- away had no socket to say so, so the reconnect compares fetched columns to
-- mounted ones and takes the same door.  Across a real remount an unsaved sheet
-- and a half-typed palette are stashed and restored, the sheet's digest re-read
-- rather than remembered.
--
-- The keys are 'Glance.Web.Keymap.keyBindingsJSON', which this parses.  Row
-- movement is the renderer's @selectStep@, which carries the column and crosses
-- a page boundary the shell is not told about; @[@ and @]@ turn a page.  The
-- column lives in the renderer's selection, so it rides along with row movement
-- and goes when the selection does.  The pill in the corner is the echo area:
-- the pending prefix, then the command and its help line.
shellGlue :: Text -> [Text]
shellGlue wanted =

  [ "  <script>"
  -- The strip is an APPEND-ONLY event log; nothing clears it, so what a reader
  -- missed is still there to scroll back to.  A line is `HH:MM:SS SEV scope
  -- message' — the stamp muted, the severity coloured, the scope one word out
  -- of a fixed set (ws, sync, cmd, filter, config, boot) naming which part of
  -- the page is talking, each part a span so it carries its own colour, and
  -- control characters in the message collapsed to spaces to keep it one line.
  -- Past LOGCAP the OLDEST line is dropped, and a line identical to the one
  -- before it bumps a counter — the only mutation an append-only strip allows,
  -- and what keeps a retry loop from filling the ring with one message.  The
  -- strip is capped in height, so keep its end in sight unless the reader has
  -- scrolled up, which is a place they are holding on purpose.
  -- One appended child: TAG under INTO, wearing CLS and holding TEXT when there
  -- is any.  Both trees this page builds — the event strip's lines and the value
  -- palette's entries — are rows of these.
  , "    const part = (into, tag, cls, text) => {"
  , "      const e = document.createElement(tag);"
  , "      e.className = cls;"
  , "      if (text !== undefined) e.textContent = text;"
  , "      into.appendChild(e);"
  , "      return e;"
  , "    };"
  , "    const LOGCAP = 500;"
  , "    let logLast = null;"
  , "    function append(scope, sev, message) {"
  , "      const box = document.getElementById(\"log\");"
  , "      const text = String(message).replace(/[\\x00-\\x1f]+/g, \" \");"
  , "      const end = box.scrollTop + box.clientHeight >= box.scrollHeight - 4;"
  , "      if (logLast && logLast.scope === scope && logLast.sev === sev"
  , "          && logLast.text === text) {"
  , "        logLast.count.textContent = `×${(logLast.n += 1)}`;"
  , "      } else {"
  , "        const line = document.createElement(\"div\");"
  , "        line.className = sev;"
  , "        part(line, \"span\", \"lt\", new Date().toTimeString().slice(0, 8));"
      -- The severity is SPELLED uppercase and WORN lowercase: the word is what
      -- a reader scans a screenful of chatter for, the class is what the
      -- stylesheet and the suite name, and the display is the only place one
      -- value folds into two cases.
  , "        part(line, \"span\", \"lv\", sev.toUpperCase());"
  , "        part(line, \"span\", \"lc\", scope);"
  , "        part(line, \"span\", \"lm\", text);"
  , "        logLast = { scope, sev, text, n: 1, count: part(line, \"span\", \"ln\", \"\") };"
  , "        box.appendChild(line);"
  , "        while (box.children.length > LOGCAP) box.removeChild(box.children[0]);"
  , "      }"
  , "      if (end) box.scrollTop = box.scrollHeight;"
  , "    }"
  , "    const el = (id) => document.getElementById(id);"
    -- THE WASH.  What is on screen stops being known to be current in exactly
    -- two ways: the view is being replaced and its answer has not landed, or
    -- the socket that would deliver a change is gone.  A reader cannot tell
    -- either from a page that is simply quiet, so both wear ONE look — faded
    -- back, never blurred, stale rows being still the rows and having to stay
    -- readable while the answer is on its way — carried by ONE class on the
    -- document element.  The event strip and the key line are exempt, being
    -- where a reader finds out why.
    --
    -- Each reason arms on a DELAY, which is the whole of what keeps the wash
    -- off a page that is working: a fetch answering inside its grace and a
    -- socket that blips and comes back dim nothing at all.  Whoever arms a
    -- reason is who clears it.
  , "    const WASH = { view: 300, socket: 400 };"
  , "    const wash = {"
  , "      n: { view: 0, socket: 0 }, at: { view: 0, socket: 0 },"
  , "      on: { view: false, socket: false },"
      -- Reason WHY now stands COUNT times over: one arming discipline for both,
      -- differing in who counts.  A view fetch STEPS the count, `load'
      -- overlapping an abort with the fetch that replaced it where a boolean
      -- would clear the wash the replacement still wants; the socket SETS it,
      -- a connection closing before it ever opened arming twice on one open.
  , "      want(why, count) {"
  , "        const was = this.n[why];"
  , "        this.n[why] = Math.max(0, count);"
  , "        if (this.n[why] === was) return;"
  , "        if (this.n[why]) this.arm(why); else this.off(why);"
  , "      },"
  , "      step(why, by) { this.want(why, this.n[why] + by); },"
  , "      arm(why) {"
  , "        if (this.on[why] || this.at[why]) return;"
  , "        this.at[why] = setTimeout(() => {"
  , "          this.at[why] = 0; this.on[why] = true; this.show();"
  , "        }, WASH[why]);"
  , "      },"
  , "      off(why) {"
  , "        clearTimeout(this.at[why]); this.at[why] = 0;"
  , "        this.on[why] = false; this.show();"
  , "      },"
  , "      show() {"
  , "        document.documentElement.classList.toggle(\"stale\","
  , "          this.on.view || this.on.socket);"
  , "      },"
  , "    };"
    -- Does MOUNT carry the optional call NAME?  Every renderer capability this
    -- page uses is detected before it is used, and there are TWO mounts now —
    -- the table and the sheet's property panel — so the question is asked of a
    -- handle rather than of the one this page used to have.
  , "    const can = (mount, name) => !!mount && typeof mount[name] === \"function\";"
    -- ROW MOVEMENT, ONCE.  Both spellings and the arrows, as a direction or
    -- zero for a key that is not one — read by the MODAL mounts, whose keys live
    -- in private listeners around the dispatch (the property panel's registers
    -- ahead of it, the rest behind).  Written once, so a third spelling is one
    -- edit and the map's own `n'/`p'/`j'/`k' rows cannot drift from what a modal
    -- surface answers to.  The table's own movement stays the map's, walking two
    -- axes and pages.
  , "    const rowStep = (k) => (k === \"<down>\" || k === \"n\" || k === \"j\" ? 1"
  , "                          : k === \"<up>\" || k === \"p\" || k === \"k\" ? -1 : 0);"
  , "    const stepIn = (mount, step) =>"
  , "      can(mount, \"selectStep\") && mount.selectStep(step);"
    -- The archive/delete flags, which are the one capability both mounts want:
    -- the table flags a row for archiving and the panel flags one for deleting,
    -- and an asset predating either says so once.
  , "    const flagsOn = (mount) => can(mount, \"flagRow\") && can(mount, \"getFlagged\");"
    -- WHERE A MOUNT'S CURSOR IS, as an id.  The renderer's own answer, asked for
    -- at the moment it matters and never kept here, and GUARDED: a mount with
    -- nothing selected answers with nothing, three surfaces read it, and an
    -- unguarded deref is a throw over an empty popup.
  , "    const selectedId = (mount) =>"
  , "      (can(mount, \"getSelection\") ? (mount.getSelection() || {}).id : null) || null;"
    -- On the next frame, or now where there are no frames.  What the panel's
    -- edit overlay waits for: the renderer stamps its selection in a frame of
    -- its own, so a row selected in this tick has no marked element yet.
  , "    const soon = (fn) =>"
  , "      (typeof requestAnimationFrame === \"function\" ? requestAnimationFrame(fn)"
  , "                                                    : setTimeout(fn, 0));"
  , "    let table = null, socket = null, backoff = 1000, editing = null;"
  , "    // The sheet's own baselines: the textarea as the file holds it as far"
  , "    // as this page knows, and the panel's drawer likewise.  The structured"
  , "    // DOCUMENT keeps none, every element in it committing on its own.  Where"
  , "    // the sheet STANDS is `subtreeSheet.state'."
  , "    let base = \"\", baseProps = null, raw = false;"
  , "    // The server filters and pages; these hold the query it was last asked"
  , "    // with, the fetch still in flight for it, and the timer that re-asks"
  , "    // when a row frame lands while one is on."
  , "    let query = \"\", inflight = null, requeryAt = 0;"
    -- WHERE POINT GOES WHEN AN ARCHIVE TAKES ITS ROW AWAY.  Armed at fire time,
    -- the last moment the view still holds the rows about to leave, and spent
    -- when they have left.  `from' is the row point was standing on, `id' the
    -- row to land on and `at' its place among the survivors; null whenever point
    -- was NOT on a leaving row, since nothing is owed then.
  , "    let leaving = null;"
    -- AND WHERE POINT GOES WHEN A CAPTURE MAKES ONE.  The mirror of `leaving',
    -- and the other half of one rule: a write that moves the view says where
    -- point is owed and the arriving rows spend it.  A capture is the one write
    -- that MAKES a row, and the id is the answer's — a minted `ORG_GLANCE_ID' for
    -- a blob, the target file's ordinal for an inbox line.
  , "    let arriving = null;"
  , "    // The tag the last answer carried, which is what makes a reconnect"
  , "    // cheap: an unmoved store answers the revalidation 304 and no rows"
  , "    // cross the wire at all."
  , "    let etag = null;"
    -- One number, two jobs: the boot asks for this many rows and the renderer
    -- shows this many at a time, so the first paint is exactly page one and
    -- the set arriving behind it only adds pages to turn to.
  , "    const PAGE = 100;   // rows in the first paint, and rows to a page"
  , "    function mount(view) {"
      -- The trail comes off the URL before the mount, because `chipLabel' can
      -- be asked for a label during the first paint: the map has to be standing
      -- when the renderer draws the chip it aliases.
  , "      const was = bootTrail();"
  , "      crumbLabels = was.labels;"
  , "      crumbSels = was.sels;"
  , "      table = TableView.mount(document.getElementById(\"app\"), view, {"
  , "        palette: true,     // the filter is summoned, never resident"
        -- The set is shown a page at a time: the renderer keeps the window,
        -- the spacers and the pager in its own status line, and movement
        -- crosses the boundary without this page knowing where one is.
  , "        pageSize: PAGE,"
        -- Marking is the renderer's chrome and the renderer's state: a
        -- checkbox column it draws and a set of ids it keys, which is why a
        -- mark outlives a filter that hides its row and a page it is not on.
        -- This page owns the keys and nothing else.
  , "        marks: true,       // dired's m/u/U/M, drawn and counted by the renderer"
        -- A flagged row's hint is the two keys that answer the flag, spelled the
        -- way the key line spells them.  The renderer draws it; an asset
        -- predating the option drops it the way it drops any other it has no
        -- field for.
  , "        flagHelp: \"d/D archive · u unflag\","
        -- The renderer's per-row hint says RET materializes, which the key line
        -- under the table already says and says for every command.  One place.
  , "        actionHints: false,"
  , "        // The applied query, restored as the renderer's own committed"
  , "        // chips. It tokenizes them and delivers nothing — the rows in"
  , "        // hand are already the server's answer to this query, and a"
  , "        // delivery here would ask for them a second time."
  , "        initialQuery: query,"
  , "        // A `ref:' chip shows what the drill was FOR, never the row id it"
  , "        // is spelled with. The query is untouched — the renderer aliases"
  , "        // the display alone — so DEL still strips the token as written."
  , "        chipLabel: (tok) => crumbLabels[tok] || null,"
  , "        onAction: (command, id) =>"
  , "          command === \"materialize\" ? materialize(id)"
  , "                                     : append(\"cmd\", \"info\","
      <> " `action: ${command}  id=${id}`),"
  , "        onLink: (target) => append(\"cmd\", \"info\", `link: ${target}`),"
  , "        onFilter: filter,   // the server narrows; the renderer shows what it is given"
  , "      });"
  , "      // An asset older than `initialQuery' drops it silently, which would"
  , "      // leave the page showing no filter over rows that are filtered."
  , "      // `getQuery()' says whether it took: when it did not, put the query"
  , "      // back in the box the way this did before chips could carry it."
  , "      if (query && !holds(query)) showQuery();"
      -- The strip goes back up the way the query did.  `setCrumbs' keeps only
      -- what parses as a crumb, so a hand-edited parameter costs the trail and
      -- nothing else.  An asset with no crumbs draws none and the labels sit
      -- unread until one arrives — a drill is refused before it starts.
  , "      if (crumbing() && was.trail.length) table.setCrumbs(was.trail);"
  , "      // The columns are the view's: both halves of a filter read the keys"
  , "      // out of them (`parity'), and cell movement names its landing column"
  , "      // by the header sitting over it."
  , "      cols = view.columns || [];"
  , "      // Whatever the remount that led here took down goes back up over the"
  , "      // new table; on a first boot there is nothing stashed and nothing to do."
  , "      restore();"
  , "    }"
  , "    // One /headlines at a time: a keystroke aborts the fetch before it, so"
  , "    // an earlier answer can never land over a later one.  TAG makes it a"
  , "    // revalidation: the browser's own cache is stepped around, so the tag"
  , "    // that goes out is this page's and the 304 comes back as the answer it"
  , "    // is rather than as a body the cache filled in behind it."
  , "    function load(params, tag) {"
  , "      if (inflight) inflight.abort();"
  , "      inflight = new AbortController();"
  , "      const init = { signal: inflight.signal };"
  , "      if (tag) { init.headers = { \"if-none-match\": tag }; init.cache = \"no-store\"; }"
  , "      return fetch(`/headlines${params}`, init).then((r) =>"
  , "        // 304: the store has not moved, so there is no view to read and the"
  , "        // rows already on screen are the current answer to this query."
  , "        r.status === 304 ? { view: null, total: 0 }"
  , "        : r.ok ? r.json().then((view) => {"
  , "            etag = r.headers.get(\"ETag\") || etag;"
  , "            return { view, total: +r.headers.get(\"X-Glance-Total\") };"
  , "          })"
  , "        // 503 is the startup walk: the server is listening and says so"
  , "        // in the body.  `start' polls it; nothing else can see it."
  , "        : r.status === 503 ? r.json().then((b) => { throw Object.assign(new Error(\"indexing\"), { indexing: b }); })"
  , "             : r.text().then((t) => { throw new Error(t); }));"
  , "    }"
  , "    const quiet = (e) => {"
  , "      if (e.name !== \"AbortError\") append(\"ws\", \"error\", `load failed: ${e.message}`);"
  , "    };"
    -- A fetch whose answer REPLACES what is on screen, marked as one: it holds
    -- the wash's view reason while it is out, so a swap slower than the grace
    -- says so rather than leaving stale rows looking current.  The parity
    -- baseline and the probe behind `@' go through `load' without this, neither
    -- replacing anything and dimming a page for a fetch that will not change it
    -- being the same lie the other way round.  A boot holds nothing either: a
    -- page with no table on it has no stale content to wash.
  , "    const viewing = (p) => {"
  , "      if (!table) return p;"
  , "      wash.step(\"view\", 1);"
  , "      return p.finally(() => wash.step(\"view\", -1));"
  , "    };"
  , "    // The unfiltered answer is kept: with a filter on, the loaded rows are"
  , "    // the server's answer to it and cannot be used to check that answer."
  , "    let all = [], cols = [];"
  , "    const paint = (a) => {"
  , "      const rows = a.view.rows || [];"
  , "      table.setRows(rows);"
  , "      if (!query) all = rows;"
  , "      parity(a.total);"
  , "    };"
  , "    // The check needs an unfiltered set to check a filtered answer against,"
  , "    // and this page can open filtered — a `?q=' link, or the default view"
  , "    // below.  A paint under a query arms nothing, so a filtered session"
  , "    // would keep the check dark for as long as it lasted.  Ask for the"
  , "    // unfiltered set once, behind everything else, keep it as the baseline"
  , "    // without touching the table, and re-run the check that had nothing to"
  , "    // run against when TOTAL was painted."
  , "    function arm(total) {"
  , "      if (!query || all.length) return;"
  , "      load(\"\").then((a) => { all = a.view.rows || []; parity(total); }).catch(quiet);"
  , "    }"
  , "    // A suggestion must never silently offer what the applied path cannot"
  , "    // evaluate.  The keys that can differ between the two halves are the"
  , "    // producer's virtual ones — the columns are in the view both read — so"
  , "    // when the server answers a query carrying one with nothing at all and"
  , "    // the words are in the rows this page already holds, say so.  Loose and"
  , "    // one-directional on purpose: it reports a suspicion and corrects"
  , "    // nothing, since guessing which half is right is how they drift."
  , "    function parity(total) {"
  , "      if (total !== 0 || !query || !all.length) return;"
  , "      if (typeof TableView.parseQuery !== \"function\") return;"
  , "      const keys = cols.map((c) => c.key);"
  , "      const loose = TableView.parseQuery(query, keys).filter((t) =>"
  , "        t.key === null && !t.quoted && !t.negated && /^[^:=]+[:=]./.test(t.value));"
  , "      if (!loose.length) return;"
  , "      const wants = loose.map((t) => t.value.slice(t.value.search(/[:=]/) + 1).toLowerCase());"
  , "      const text = (r) => keys.map((k) => TableView.displayText((r.cells || {})[k]))"
  , "        .join(\"\\x1f\").toLowerCase();"
  , "      const local = all.filter((r) => wants.every((v) => text(r).includes(v))).length;"
  , "      if (!local) return;"
  , "      const note = \"filter parity divergence — asset/daemon version skew\";"
  , "      console.warn(note, { query, server: total, local });"
  , "      append(\"filter\", \"warn\", note);"
  , "      echo(note);"
  , "    }"
  , ""
  , "    // The applied query is page state.  It rides in the URL, so a filtered"
  , "    // view is a link and a reconnect comes back to it; DEL takes its last"
  , "    // token off through the renderer.  The shell sends the string as typed"
  , "    // — the grammar is the server's to parse (SCHEMA.md)."
  , "    const params = () => new URLSearchParams(location.search);"
  , "    const urlQuery = () => params().get(\"q\") || \"\";"
  , "    // What the page opens on when the address bar says nothing, and what"
  , "    // `g' applies.  The daemon embeds it at request time out of the tree's"
  , "    // own `#+GLANCE_DEFAULT_FILTER:', falling back to org-glance's spelling"
  , "    // of the active group.  A `?q=' is the user's intent whatever it holds,"
  , "    // an empty one included, so the default is injected only where there is"
  , "    // no `q' at all — and then it is a query like any other, committed to"
  , "    // the URL, shown as the renderer's chip and asked of the server."
  , "    const DEFAULT_QUERY = " <> jsonValue wanted <> ";"
  , "    const bootQuery = () => (params().has(\"q\") ? urlQuery() : DEFAULT_QUERY);"
    -- The drill-down trail.  The STACK is the renderer's — it draws the crumbs,
    -- and `setView' drops them with the world they described — so this page
    -- keeps no copy of it and reads it back when it needs one, the way it keeps
    -- no copy of the marks or of the selected column.  What it does keep is the
    -- LABEL a `ref:' token wears, since no lookup can recover it: the title
    -- belongs to the row referred TO, which is very rarely among its own
    -- referrers, so by the time the drill has landed the title is nowhere in the
    -- rows on screen.  Keyed by the token, so one map answers both readers —
    -- `chipLabel' aliasing the live chip, and the crumb a further drill leaves.
  , "    let crumbLabels = {};"
  , "    const crumbing = () => can(table, \"pushCrumb\") && can(table, \"popCrumb\")"
  , "      && can(table, \"getCrumbs\") && can(table, \"setCrumbs\");"
  , "    const trail = () => (crumbing() ? table.getCrumbs() : []);"
    -- The selection each crumb was pushed FROM, one entry per crumb.  It rides
    -- BESIDE the trail rather than inside it, the renderer's `crumbOf' keeping a
    -- crumb's `label' and `query' and dropping everything else, so a selection
    -- put in a crumb would never come back out of `getCrumbs()'.  The renderer's
    -- DEPTH is still the truth: a side table fallen out of step with it is
    -- dropped whole rather than pairing a crumb with another crumb's row.
  , "    let crumbSels = [];"
  , "    const selsFit = () => crumbSels.length === trail().length;"
    -- Where a landing puts the cursor.  ONE function, three rules differing only
    -- in what they ask for: an APPLIED view — a palette commit, `g', `a', `@' —
    -- asks for nothing and takes the FIRST row of the answer, a POP asks for the
    -- row its drill was launched from, and an ARCHIVE asks for the row after the
    -- ones it took away, at the place they left.  An empty answer selects
    -- nothing, whichever asked.  `select' answers false for a row the view no
    -- longer holds, so a remembered row an edit or a narrower filter took away
    -- falls through to AT — index 0 for the two callers that name none, the
    -- first-row landing spelled as the general rule rather than beside it.  The
    -- COLUMN rides across either landing: a commit repaints the same mount, so
    -- the cell the reader was reading in is still there to land in, and `^',
    -- which is a commit now, would otherwise take the selection it needs away
    -- from the next press of itself.  After a REMOUNT there is no column to keep
    -- and `column()' answers null, the whole-row look this landed on before.
  , "    function land(sel, back) {"
  , "      if (!can(table, \"select\")) return;"
  , "      const rows = visible();"
  , "      if (!rows.length) return;"
  , "      if (sel && sel.id"
  , "          && table.select(sel.id, sel.col === null ? undefined : sel.col)) return;"
  , "      const at = column();"
  , "      const i = Math.max(0, Math.min(back || 0, rows.length - 1));"
  , "      table.select(rows[i].id, at === null ? undefined : at);"
  , "    }"
    -- A row as the `ref:' token naming it.  The value is quoted where the id
    -- carries a token separator: the fallback row id is `PATH#K' and a path may
    -- hold a space, which the grammar would otherwise cut the token at.  An id
    -- carrying a QUOTE is beyond this — the scanner drops quote characters
    -- rather than unescaping them — and no id spelling seen in the corpus does.
  , "    const refToken = (id) => `ref:${/[\\s&\"]/.test(id) ? `\"${id}\"` : id}`;"
    -- What the view being LEFT is called, for the crumb that stands in for it.
    -- A labelled jump chains honestly: drilling out of a drill leaves the first
    -- drill's own name behind rather than its `ref:' spelling, and any other
    -- query is its own best name.
  , "    const hereLabel = () => crumbLabels[query] || query || \"all rows\";"
  , "    // Every applied query is written, the EMPTY one included: a `q' that is"
  , "    // present and empty is a reader who took the filter off, where an absent"
  , "    // one is a page nobody has filtered yet.  Only the second has the default"
  , "    // injected over it, so DEL'ing the last chip survives a reload and every"
  , "    // remount after it — deleting the parameter here is what made a cleared"
  , "    // view come back filtered."
  , "    //"
  , "    // The trail rides beside it, and the URL is the ONLY channel it crosses"
  , "    // a remount by: every mutation of the stack — a drill, a pop, `g' — is"
  , "    // followed by a `remember', so the address bar is current whenever"
  , "    // `mount' reads it back.  That is why `stash'/`restore' say nothing"
  , "    // about crumbs: what they carry is work the reader has NOT committed,"
  , "    // and there is no such thing as a half-applied crumb."
  , "    function remember(q) {"
  , "      const p = params();"
  , "      p.set(\"q\", q);   // `keys' and anything else in the URL survives"
  , "      const t = trail(), labels = Object.keys(crumbLabels).length ? crumbLabels : null;"
  , "      if (!t.length && !labels) p.delete(\"crumbs\");"
  , "      else p.set(\"crumbs\", JSON.stringify("
      <> " { trail: t, labels: crumbLabels, sels: selsFit() ? crumbSels : [] }));"
  , "      history.replaceState(null, \"\", `?${p.toString()}`);"
  , "    }"
    -- The trail as the address bar carries it.  A parameter a hand has been in
    -- is not worth a diagnostic: anything that does not parse into the two
    -- fields is one boot without a trail, which is where a reader starts anyway.
  , "    function bootTrail() {"
  , "      try {"
  , "        const was = JSON.parse(params().get(\"crumbs\") || \"null\");"
  , "        if (!was || typeof was !== \"object\")"
  , "          return { trail: [], labels: {}, sels: [] };"
  , "        return {"
  , "          trail: Array.isArray(was.trail) ? was.trail : [],"
  , "          labels: was.labels && typeof was.labels === \"object\" ? was.labels : {},"
  , "          sels: Array.isArray(was.sels) ? was.sels : [],"
  , "        };"
  , "      } catch (e) { return { trail: [], labels: {}, sels: [] }; }"
  , "    }"
  , "    // A query as the `/headlines' query string asking it, spelled once for"
  , "    // the four callers that want it — the boot, a commit, the arming fetch"
  , "    // and the reconnect.  A second spelling is how a revalidation comes to"
  , "    // be answered 304 against rows answering some other question."
  , "    const asking = (q) => (q ? `?q=${encodeURIComponent(q)}` : \"\");"
  , "    // One place asks the server for rows: `query' is already what to ask."
      -- A COMMIT is a new question, so the cursor has no claim on the answer: it
      -- REPAINTS rather than remounting and would otherwise stay wherever it was,
      -- on a row the new answer may not hold, so it takes the same first-row
      -- landing `applyView' gives every applied view.  A REFETCH THE WATCH CAUSED
      -- is the view the reader already had, arriving again because a file moved,
      -- so it lands nothing of its own: the renderer keeps the cursor where it
      -- was — on its row while that row is still there, else at the same visual
      -- place — and only the archive that took the rows away may override that,
      -- saying so by arming `leaving'.  That carve is what stops somebody else's
      -- edit yanking a reader back to row one.
  , "    const fetchRows = (landing) =>"
  , "      viewing(load(asking(query)))"
  , "        .then((a) => { if (!table) return;"
  , "                       paint(a);"
  , "                       if (landing) landing(); else land(null); })"
  , "        .catch(quiet);"
  , "    // A commit is the moment a NEW query goes to the server — a settled"
  , "    // debounce, a committed token, an accepted completion."
  , "    function commit(q) {"
  , "      if (q === query) return;"
  , "      query = q;"
  , "      leaving = arriving = null;   // both belonged to the view being left"
  , "      remember(q);"
  , "      fetchRows();"
  , "    }"
  , "    const filter = (q) => commit(q.trim());"
  , "    // The query's last token comes off in the renderer, which owns the"
  , "    // chips showing it: a shell-side strip would leave them on screen"
  , "    // spelling a filter that is no longer applied.  An asset too old to"
  , "    // have the pair says so rather than growing a second implementation."
  , "    const strips = () => can(table, \"stripLastToken\") && can(table, \"getQuery\");"
  , "    // Whether the mounted renderer is carrying Q as its own query."
  , "    const holds = (q) => can(table, \"getQuery\") && table.getQuery() === q;"
  , "    // The renderer's filter field, wherever its mode puts it: the palette's"
  , "    // input in palette mode, the resident box in an asset predating one."
  , "    // Named once, since three callers want it and none of them may reach"
  , "    // further into the chrome than this."
  , "    const filterBox = () => document.querySelector(\"#app .tv-filter\");"
  , "    // The fallback for an asset without `initialQuery': the query goes in"
  , "    // the box rather than into chips.  The box is the renderer's, and"
  , "    // setting its value fires no input event, so a restored query shown"
  , "    // there is not committed a second time."
  , "    function showQuery() {"
  , "      const box = filterBox();"
  , "      if (box) box.value = query;"
  , "    }"
  , ""
  , "    // An answer unwrapped, with the server's own error thrown: the routes"
  , "    // that read a value want one handling of a refusal, so the shape sits"
  , "    // here once and both doors below take it."
  , "    const unwrap = (r) => r.json().then((b) => {"
  , "      if (!r.ok) throw new Error(b.error || r.status);"
  , "      return b;"
  , "    });"
  , "    const getJSON = (url) => fetch(url).then(unwrap);"
  , "    // And a JSON POST: the method, the one header and the encoding decided"
  , "    // once, for every route that takes a body.  EXTRA is what a page closing"
  , "    // on an edited sheet adds — `keepalive', being the one caller that"
  , "    // cannot wait."
  , "    const postJSON = (url, body, extra) =>"
  , "      fetch(url, {"
  , "        method: \"POST\","
  , "        headers: { \"content-type\": \"application/json\" },"
  , "        body: JSON.stringify(body),"
  , "        ...extra,"
  , "      });"
  , "    // What a WRITE answers, status and body together: a 409 carries a body"
  , "    // saying which kind it is, so both are read rather than the status alone."
  , "    const outcome = (r) => r.json().then((b) => ({ status: r.status, body: b }));"
  , ""
  , "    // The two shapes of /headline, each written once.  `post' pins"
  , "    // the write to DIGEST."
    -- The route's own address, and the ONE place this page spells it: a row id,
    -- and the index of an entry inside that row's subtree where the sheet has
    -- walked into one.  A child is a number the SERVER handed over — every
    -- answer names the entries under it and the one above it — so this page
    -- counts no stars and holds no outline of its own.
  , "    const at = (id, child) => `/headline?id=${encodeURIComponent(id)}`"
  , "      + (child === null || child === undefined ? \"\" : `&child=${child}`);"
  , "    const headline = (id, child) => getJSON(at(id, child));"
  , "    const post = (id, digest, asked, extra, child) =>"
  , "      postJSON(at(id, child), { ...asked, digest }, extra);"
  , "    function materialize(id) {"
  , "      headline(id).then((h) => show(h, false))"
  , "        .catch((e) => append(\"sync\", \"error\", `materialize failed: ${e.message}`));"
  , "    }"
  , "    // ONE PANE over one subtree, and `raw' says which one is showing.  The"
  , "    // structured document is the resident shape and commits per element; the"
  , "    // textarea is the escape hatch, and it keeps the buttonless ladder —"
  , "    // `base' is what the file holds as far as this page knows, and `dirty()'"
  , "    // over it is the whole of what decides whether closing costs a POST."
  , "    function show(h, asRaw) {"
  , "      editing = h; raw = !!asRaw;"
  , "      el(\"mfile\").textContent = `${h.file}  ·  ${h.id}`;"
  , "      fill(h);"
  , "      sync(\"synced\");"
  , "      el(\"modal\").className = \"on\";"
      -- Raw mode is a textarea and takes the focus; the document holds the keys
      -- with NOTHING focused, the way the panel's nav did — which is what leaves
      -- every printable key free to be movement and a command.
  , "      if (raw) el(\"mtext\").focus(); else el(\"mtext\").blur();"
  , "    }"
  , "    // Both panes filled from H.  The document keeps NO baseline — every"
  , "    // element in it commits on its own — so what `dirty()' is measured"
  , "    // against is the panel's own model and, in raw mode, the textarea."
  , "    function fill(h) {"
  , "      base = raw ? h.org : \"\";"
  , "      el(\"mtext\").value = base;"
      -- TOGGLE, never assign.  The sheet's class carries its SIZE TIER as well
      -- as its shape, and a wholesale write drops the tier on the first
      -- materialize — silently, since the markup still reads right and only a
      -- live page is a size.  `classList' spells "set one class, keep the rest",
      -- keeping the tier a fact of the element rather than a string to respell.
  , "      el(\"sheet\").classList.toggle(\"raw\", raw);"
  , "      shutEdit(DROW); shutEdit(DPARA);"
  , "      dflags.clear();"
    -- THE LINKS COME WITH THE MATERIALIZE, since the display needs them: one
    -- `/links' beside the `/headline' that opened the sheet.  The document is
    -- drawn without waiting and drawn again when the answer lands, so a slow or
    -- a failed link scan costs the marks and never the sheet.
  , "      dlinks = [];"
  , "      if (raw) { drows = []; dlines = []; drawDoc(); } else docFrom(h);"
  , "      if (!raw) linksOf(h.id).then((a) => {"
  , "        if (editing && editing.id === h.id) { dlinks = a.links || []; drawDoc(); }"
  , "      }).catch(() => {});"
  , "      drawProps(raw ? [] : h.properties || [], raw ? [] : h.planning || []);"
  , "      el(\"mdoc\").className = raw ? \"\" : \"on\";"
  , "      drawWhere(h.path || []);"
  , "      drawLog(raw ? \"\" : h.logbook || \"\");"
  , "      baseProps = raw ? null : edited();"
  , "    }"
    -- Everything the panel holds, as one string to compare against.  Two lists
    -- rather than one, so a property and a planning entry spelling the same pair
    -- cannot cancel out.
  , "    const edited = () => JSON.stringify([props(), planning()]);"
    -- THE SHEET'S OWN CRUMB STRIP, the drill stack's rhyme one level in: the
    -- table leaves a crumb when `@' drills into a reference, the sheet leaves one
    -- when `RET' drills into a child, and `DEL' walks both back, so both draw the
    -- same thing.  STANDING, so it is a place rather than a notification — the
    -- ROW alone is one crumb and each descent appends, where a strip appearing on
    -- the way down would move the panes under the reader as they arrived.
    --
    -- It wears the renderer's own MUTED CHIP — same silhouette, dimmed ink:
    -- `.tv-chip' plus `.tv-chip-muted', hand-copied the way `--g-border' is,
    -- those rules living inside `.tv-root' where nothing outside a mount reaches
    -- them.  The LAST crumb is where the reader stands and takes the full ink,
    -- the one thing the strip says that the renderer's own crumbs do not have to.
    -- Inert: `DEL' is the climb and the key line teaches it, so there is nothing
    -- to click and no hint crowding the bar.
  , "    function drawWhere(path) {"
  , "      const bar = el(\"mwhere\");"
  , "      bar.textContent = \"\";"
  , "      path.forEach((title, i) =>"
  , "        part(bar, \"span\", \"wc\" + (i === path.length - 1 ? \" wat\" : \"\"),"
  , "             title || \"(untitled)\"));"
  , "    }"
    -- The logbook strip: shown, never sent, and taken off the sheet outright
    -- when there is none rather than left as a labelled blank.  The drawer's
    -- INTERIOR alone — `:LOGBOOK:' and `:END:' delimit the thing the widget
    -- already is, so showing them spends two of the strip's lines saying what the
    -- strip is.  The cut is display-only: what goes back into the file is the
    -- whole drawer, delimiters and all, and this page never sends it at all.
  , "    function drawLog(text) {"
  , "      const inner = text.replace(/\\n$/, \"\").split(\"\\n\").slice(1, -1).join(\"\\n\");"
  , "      el(\"mlog\").textContent = inner;"
  , "      el(\"mlog\").className = inner ? \"on\" : \"\";"
  , "    }"
    -- DIRTY IS THE PANEL'S AND RAW MODE'S.  The structured document commits per
    -- ELEMENT — each write its own drift-locked splice, each answer re-pinning
    -- the digest — so it never holds work nobody wrote; the panel's model and the
    -- textarea are the two that can, keeping the whole ladder they always had:
    -- flush on the way out, `conflict' and `error' waiting for a keystroke.
  , "    const dirty = () => editing !== null"
  , "      && (raw ? el(\"mtext\").value !== base : edited() !== baseProps);"
    -- THE STRUCTURED DOCUMENT, the sheet's LEFT pane, standing where the textarea
    -- did.  A subtree's TEXT is a HEADLINE LINE with cells, body paragraphs and
    -- the children hanging under it, so the sheet draws those in file order and
    -- the cursor walks them; the drawer and the planning line stay the PANEL's
    -- beside it, being a list of records, and the renderer draws every list here.
    --
    -- FLOWING TEXT until the cursor lands: nothing is boxed, ruled or labelled
    -- while it is being read, the ELEMENT under point wears the page's own
    -- selection exactly as a table row does, and its parts show their names only
    -- while it is being worked on — so what a reader sees when they are not
    -- editing is the entry as org spells it.
    --
    -- NO TABLE-VIEW MOUNT, the one place on the page with none.  The renderer's
    -- list widget draws a list of RECORDS — one column table over rows of one
    -- shape — where this is a list of KINDS, five of them sharing no columns, so
    -- a mount would need a column table fitting none of them and a per-row shape
    -- the renderer has no field for.
    --
    -- MODEL AND VIEW.  `drows' is the model — one entry per element, each
    -- carrying what it HOLDS — and `drawDoc' is the whole of the view.  A commit
    -- moves the model, so an open edit is not a change and cannot be written:
    -- the fields hold the edit and the element holds the committed text.
    --
    -- PER-ELEMENT COMMITS: every element in this pane writes on its own — a lens
    -- splice for a paragraph, a `/command' for the headline's own cells — each
    -- under the file's digest and each re-pinning it from the answer.  So nothing
    -- here is ever unsaved, and the sheet's dirty ladder is the PANEL's alone
    -- (and raw mode's), exactly as it was.
  , "    const DCELLS = " <> jsonValue (["state", "priority", "title", "tags"] :: [Text]) <> ";"
    -- The model, the cursor and the body's own lines.  GRAIN is reserved: the
    -- cursor covers one ELEMENT today and the field is what a future
    -- expand-region moves — a paragraph's line, a subtree, the whole document —
    -- without every reader of the cursor learning about it twice.
  , "    let drows = [], dat = 0, dcol = null, dgrain = \"element\";"
  , "    let dlines = [];"
    -- The ELEMENT the draw put the cursor on, kept so the edit overlay can be
    -- anchored to it.  The `dat'-th child of `#dlist' is NOT that element: a
    -- composite draws its leaves INSIDE it, so the two stop agreeing at the
    -- first list or block in the document and every edit below one anchored to
    -- the wrong element.  The draw is what knows which box it marked.
  , "    let dcursor = null;"
    -- The flags are the document's own, keyed by element id the way the
    -- renderer's are keyed by row id — a Set and four calls, which is exactly
    -- what `flagKey' feature-detects, so the deletion gesture is the page's one
    -- implementation over a fourth surface.
  , "    const dflags = new Set();"
  , "    const dmount = {"
      -- Each of the three that MOVES a flag redraws, since the wash is the
      -- draw's: a mount would have repainted itself and this widget is the
      -- page's own, so the redraw is where the set is written.
  , "      flagRow: (id) => { dflags.add(id); drawDoc(); },"
  , "      unflagRow: (id) => { dflags.delete(id); drawDoc(); },"
  , "      getFlagged: () => [...dflags],"
  , "      clearFlags: () => { dflags.clear(); drawDoc(); },"
  , "    };"
    -- H's four cells, in the order org writes them on a headline line.  One
    -- reading for the headline itself and for every child, since a child line IS
    -- a headline line drawn one level in.
  , "    const cellsOf = (o) => DCELLS.map((k) => ({ key: k, val: (o || {})[k] || \"\" }));"
    -- The body's PARAGRAPHS: runs of non-blank lines, each remembering the LINE
    -- RANGE it came out of.  The range is what makes an edit a splice — a commit
    -- puts the paragraph's own lines back where they were and leaves every other
    -- byte of the body alone, blank lines and odd spacing included.  Line 0 is
    -- the headline's own and is never one of them (`blocksIn').
    -- ORG'S LIST OPENERS, as the corpus actually spells them: `- ' at 28571
    -- lines, `1.'/`1)' at 2675, `+ ' at 42 and an INDENTED `* ' at 34.  All four
    -- are honoured because all four cost one alternation.  A `* ' at COLUMN 1 is
    -- a headline rather than an item — the lens has taken those out already, but
    -- the guard is kept here so the predicate is true on its own terms.
  , "    const LIST_AT = /^(\\s*)([-+*]|\\d+[.)])(\\s+|$)/;"
  , "    function opener(line) {"
  , "      const m = LIST_AT.exec(String(line));"
  , "      return m && !(m[2] === \"*\" && !m[1]) ? m : null;"
  , "    }"
    -- A BLOCK IS ANY `#+begin_X'/`#+end_X' PAIR, by name.  Naming quote, src and
    -- example outright would have missed this corpus's most common block by a
    -- factor of three: `pin' 1022, `src' 338, `quote' 111, `notes' 42,
    -- `example' 38.  A reader's own block kind is as much a block as org's.
  , "    const BEGIN_AT = /^\\s*#\\+begin_(\\S+)/i;"
  , "    const closerOf = (name) => new RegExp("
  , "      \"^\\\\s*#\\\\+end_\" + name.replace(/[.*+?^${}()|[\\]\\\\]/g, \"\\\\$&\")"
  , "        + \"\\\\s*$\", \"i\");"
    -- Where the block ENDS, or nothing: an opener with no closer under it is
    -- ordinary text, since guessing an end would put a stop around bytes org
    -- itself reads as a paragraph.
  , "    function blockRun(lines, i, end) {"
  , "      const shut = closerOf(BEGIN_AT.exec(lines[i])[1]);"
  , "      for (let j = i + 1; j < end; j += 1) if (shut.test(lines[j])) return j + 1;"
  , "      return -1;"
  , "    }"
    -- AN ORG TABLE IS A RUN OF `|' LINES, the opening bar being the whole of what
    -- says so: org's own rule (`org-table-any-line-regexp'), the same one that
    -- makes a `|---+---|' RULE a row of the table rather than an end to it.  A
    -- run of them is ONE COARSE STOP with its lines under it, which is the LIST'S
    -- shape exactly — `[whole, row1..rowN]' — so the walk, the draw, the flags,
    -- `o' and the splice all reach it through what is already there.
    -- A LINE IS A LEAF, and that is the whole grain: 101 of this corpus's 6337
    -- files hold table rows (2178 lines, 211 of them rules), so a table is real
    -- and rare and a cell grain would be a second walk to teach for one file in
    -- sixty.  Editing a row is editing its line, which is what org's own table
    -- editor comes to once the alignment is left to org.
  , "    const TABLE_AT = /^\\s*\\|/;"
    -- What stays INSIDE a list once it has opened: another item at any depth, or
    -- an indented continuation line.  An unindented line that is not an item
    -- ends it.
  , "    const rides = (line) => !!opener(line) || /^\\s/.test(String(line));"
    -- A LIST RUN and its TOP-LEVEL items.  The base indent is the FIRST item's,
    -- and an item deeper than it rides inside the item above rather than taking a
    -- stop of its own — v1's grain, and the nesting is still there in the text.
    --
    -- ONE BLANK LINE STAYS IN, which is org's own rule and the corpus's: 1173
    -- item pairs are separated by exactly one.  Two, or a blank with something
    -- else under it, close the list.
  , "    function listRun(lines, i, end) {"
  , "      const base = opener(lines[i])[1].length;"
  , "      const items = [];"
  , "      let at = i, from = -1, last = i;"
  , "      while (at < end) {"
  , "        if (String(lines[at]).trim() === \"\") {"
  , "          let j = at;"
  , "          while (j < end && String(lines[j]).trim() === \"\") j += 1;"
  , "          if (j - at > 1 || j >= end || !rides(lines[j])) break;"
  , "          at = j; continue;"
  , "        }"
  , "        const m = opener(lines[at]);"
  , "        if (m && m[1].length <= base) {"
  , "          if (from !== -1) items.push({ from, to: last });"
  , "          from = at;"
  , "        } else if (!rides(lines[at])) break;"
  , "        at += 1; last = at;"
  , "      }"
  , "      if (from !== -1) items.push({ from, to: last });"
  , "      return { to: last, items };"
  , "    }"
    -- Blank-separated runs of A..B, which is what a plain paragraph is and what
    -- a block's interior is cut into.
  , "    function runsIn(lines, a, b) {"
  , "      const out = [];"
  , "      let from = -1;"
  , "      for (let i = a; i <= b; i += 1) {"
  , "        const blank = i === b || String(lines[i]).trim() === \"\";"
  , "        if (!blank) { if (from === -1) from = i; continue; }"
  , "        if (from === -1) continue;"
  , "        out.push({ from, to: i, text: lines.slice(from, i).join(\"\\n\") });"
  , "        from = -1;"
  , "      }"
  , "      return out;"
  , "    }"
    -- THE WALK SEQUENCE, in document order, with the COMPOSITES INLINE: a list,
    -- a block or a table is `[whole, part1..partN]', so `n' from above meets the
    -- whole thing first and then walks into it, and `p' from below walks the
    -- parts and meets the whole on the way out.  That falls out of one flat list
    -- — there is no descend key and no ascend key, and `p' is `n' read backwards
    -- because the sequence is the same sequence.
    --
    -- A plain paragraph is one stop, as it always was.  Line 0 is the headline's
    -- own, which the lens leaves at the head of the body: it is the headline
    -- ELEMENT's, drawn from the cells the server sent, and it is never a
    -- paragraph.  OWN is where this entry's own text stops and the outline under
    -- it begins — the server's `ownLines', since the page holds no parser and the
    -- same bytes must not be drawn twice, once as a paragraph and once as the
    -- child that owns them.
  , "    function blocksIn(lines, own) {"
  , "      const out = [];"
  , "      const end = Math.max(0, Math.min(own, lines.length));"
  , "      const cut = (a, b) => lines.slice(a, b).join(\"\\n\");"
  , "      const whole = (a, b, name, leaves) => {"
  , "        out.push({ from: a, to: b, text: cut(a, b), grain: \"composite\", name });"
  , "        for (const p of leaves)"
  , "          out.push({ from: p.from, to: p.to, text: p.text, grain: \"leaf\" });"
  , "      };"
  , "      let i = 1;"
  , "      while (i < end) {"
  , "        if (String(lines[i]).trim() === \"\") { i += 1; continue; }"
  , "        if (BEGIN_AT.test(lines[i])) {"
  , "          const shut = blockRun(lines, i, end);"
  , "          if (shut !== -1) {"
  , "            whole(i, shut, BEGIN_AT.exec(lines[i])[1].toLowerCase(),"
  , "                  runsIn(lines, i + 1, shut - 1));"
  , "            i = shut; continue;"
  , "          }"
  , "        }"
      -- Every line its own leaf, which is the one place a table differs from a
      -- list: a list's items are RUNS and a table's rows are LINES, so the
      -- leaves are cut here rather than by a scan of what rides inside them.
  , "        if (TABLE_AT.test(lines[i])) {"
  , "          let j = i;"
  , "          while (j < end && TABLE_AT.test(lines[j])) j += 1;"
  , "          const rows = [];"
  , "          for (let n = i; n < j; n += 1)"
  , "            rows.push({ from: n, to: n + 1, text: cut(n, n + 1) });"
  , "          whole(i, j, \"table\", rows);"
  , "          i = j; continue;"
  , "        }"
  , "        if (opener(lines[i])) {"
  , "          const run = listRun(lines, i, end);"
  , "          whole(i, run.to, \"list\", run.items.map("
  , "            (it) => ({ from: it.from, to: it.to, text: cut(it.from, it.to) })));"
  , "          i = run.to; continue;"
  , "        }"
      -- A paragraph stops where the next STRUCTURE opens as readily as at a
      -- blank line: org lets a list follow its lead-in with no blank between.
  , "        let j = i + 1;"
  , "        while (j < end && String(lines[j]).trim() !== \"\""
  , "               && !opener(lines[j]) && !BEGIN_AT.test(lines[j])"
  , "               && !TABLE_AT.test(lines[j])) j += 1;"
  , "        out.push({ from: i, to: j, text: cut(i, j), grain: \"element\" });"
  , "        i = j;"
  , "      }"
  , "      return out;"
  , "    }"
    -- The document H makes, in FILE ORDER: the headline line, the body's own
    -- paragraphs, then the children hanging under it.  The cursor is kept BY ID
    -- across a rebuild, so a commit that re-materializes lands the reader back on
    -- the element they were working on rather than at the top.
  , "    function docFrom(h) {"
      -- ORG-STARTUP-INDENTED'S OTHER HALF: content lines sit under the TITLE
      -- TEXT rather than under the stars.  The head always draws `* ', being the
      -- root of its own document whatever entry the sheet walked into, so the
      -- column is the width of that prefix and is DERIVED from `dstars' rather
      -- than spelled as a 2 beside it, which keeps the two rules from drifting.
      -- Written as a NUMBER onto the pane, the way the log's cap is: the
      -- arithmetic lives in the stylesheet, once.
  , "      el(\"mdoc\").style.setProperty(\"--g-doc-indent\","
  , "                                    String(dstars(docLevel()).length));"
  , "      const was = drows[dat] ? drows[dat].id : null;"
  , "      drows = [];"
  , "      dlines = String(h.body || \"\").split(\"\\n\");"
  , "      drows.push({ id: \"H\", kind: \"head\", cells: cellsOf(h.cells) });"
  , "      const own = h.ownLines === undefined ? dlines.length : h.ownLines;"
      -- AN ELEMENT ID IS ITS PLACE IN THIS BUILD, re-issued every time the
      -- document is built: `B0' is the first paragraph of whatever the body holds
      -- NOW.  The cursor, the flags and a stash restore are all keyed by it, so
      -- an edit that changes how many elements sit ABOVE one moves that one's id
      -- and a reader lands on whatever now carries the ordinal.  Stable enough
      -- for what reads it — a build lasts from one commit to the next, and a
      -- commit re-materializes and re-draws in one step — and a different rule
      -- from the panel's, where `P<n>' is handed out once and nothing rebuilds
      -- the drawer under the reader.  The counter is the loop's, that being the
      -- whole of its life.
  , "      let owner = null, seq = 0;"
  , "      for (const b of blocksIn(dlines, own)) {"
  , "        const id = `B${seq++}`;"
  , "        owner = b.grain === \"composite\" ? id : b.grain === \"leaf\" ? owner : null;"
  , "        drows.push({ id, kind: \"para\", grain: b.grain, name: b.name || null,"
  , "                     owner: b.grain === \"leaf\" ? owner : null,"
  , "                     from: b.from, to: b.to, text: b.text, was: b.text });"
  , "      }"
  , "      for (const c of h.children || [])"
  , "        drows.push({ id: `C${c.index}`, kind: \"child\", index: c.index,"
  , "                     level: c.level, cells: cellsOf(c) });"
  , "      const back = drows.findIndex((r) => r.id === was);"
  , "      dat = back === -1 ? 0 : back;"
  , "      dcol = null;"
  , "      drawDoc();"
  , "    }"
    -- The BODY a commit sends: the lines as they were, with each paragraph that
    -- MOVED spliced back over its own range and each one DROP names taken out.
    -- Bottom-up, so an earlier range is never moved by a later splice.  A
    -- deletion eats the blank line that separated the block from the next one,
    -- or a document would collect blank lines one delete at a time.
  , "    function bodyText(drop) {"
  , "      const gone = drop || new Set();"
  , "      const out = dlines.slice();"
    -- ONE GRAIN SPEAKS FOR A RANGE.  A composite and its leaves cover the same
    -- lines, so a commit must not splice both: a composite that MOVED, or that
    -- is going, answers for everything inside it and its leaves are left out.
    -- Which is why a reader flagging a list and one of its items still gets one
    -- deletion rather than a corrupted body.
  , "      const spoken = new Set(drows.filter((r) => r.grain === \"composite\""
  , "        && (gone.has(r.id) || r.text !== r.was)).map((r) => r.id));"
  , "      const paras = drows.filter((r) => r.kind === \"para\""
  , "        && !spoken.has(r.owner)).slice().reverse();"
  , "      for (const p of paras) {"
  , "        if (gone.has(p.id)) {"
  , "          const spare = p.to < out.length - 1 && String(out[p.to]).trim() === \"\";"
  , "          out.splice(p.from, p.to - p.from + (spare ? 1 : 0));"
  , "        } else if (p.text !== p.was) {"
  , "          out.splice(p.from, p.to - p.from, ...p.text.split(\"\\n\"));"
  , "        }"
  , "      }"
  , "      return out.join(\"\\n\");"
  , "    }"
    -- ONE DRAW, and it is the whole view.  Every element is a row of `#dlist'
    -- wearing its KIND as a class, the one under point wearing `dat' — the
    -- page's own selection, the same cursor language the table draws — and a
    -- flagged element `dfl' beside it.
    --
    -- A PART THE HEADLINE HAS NOT GOT RENDERS NOTHING, in every state: no
    -- placeholders and no reserved gaps, so what a reader sees is the entry as
    -- org spells it and what marks structure is the CURSOR alone.  Setting an
    -- absent part is the COMMANDS' job — `t' and `:' at the element — rather
    -- than a cell that has to be visible before it can be walked onto.
    -- WHAT A STOP LOOKS LIKE: its kind as a class, `dat' on the one under point,
    -- `dfl' on a flagged one.  A leaf is `d-item' and a composite `d-comp'
    -- beside its own name, so a list and a block are stylable apart without the
    -- draw knowing which is which.
  , "    const dclass = (r, here) => `de d-${r.grain === \"leaf\" ? \"item\""
  , "      : r.grain === \"composite\" ? `comp d-${r.name}` : r.kind}`"
  , "      + (here ? \" dat\" : \"\") + (dflags.has(r.id) ? \" dfl\" : \"\");"
  , "    function drawDoc() {"
  , "      const list = el(\"dlist\");"
  , "      list.textContent = \"\";"
  , "      dcursor = null;"
  , "      for (let i = 0; i < drows.length; i += 1) {"
  , "        const r = drows[i];"
  , "        const here = i === dat;"
  , "        const row = part(list, \"div\", dclass(r, here));"
  , "        if (here) dcursor = row;"
        -- A COMPOSITE IS DRAWN ONCE, with its leaves INSIDE it: the walk has two
        -- stops over one range and the reader must see one list.  Its leaves are
        -- the rows straight after it, so the draw walks them here and the outer
        -- loop steps past what it took.  What no leaf claims is drawn as an INERT
        -- run — the `#+begin_' and `#+end_' lines, the blank line between two
        -- items, a lead-in the list's own opener did not take — so every byte the
        -- composite covers is on screen exactly once, the lens's rule one grain
        -- down.
  , "        if (r.grain === \"composite\") {"
  , "          let at = r.from, j = i + 1;"
  , "          for (; j < drows.length && drows[j].owner === r.id; j += 1) {"
  , "            const leaf = drows[j];"
  , "            if (leaf.from > at)"
  , "              part(row, \"div\", \"dg\", dlines.slice(at, leaf.from).join(\"\\n\"));"
  , "            const box = part(row, \"div\", dclass(leaf, j === dat));"
  , "            drawPara(box, leaf);"
  , "            if (j === dat) dcursor = box;"
  , "            at = leaf.to;"
  , "          }"
  , "          if (at < r.to)"
  , "            part(row, \"div\", \"dg\", dlines.slice(at, r.to).join(\"\\n\"));"
  , "          i = j - 1;"
  , "        } else if (r.kind === \"para\") drawPara(row, r);"
  , "        else drawCells(row, r, here);"
  , "      }"
  , "      keepInView(dcursor);"
  , "      placeEdit();"
  , "    }"
    -- THE CURSOR STAYS IN VIEW, the TABLE's own discipline over the pane this
    -- page owns: `#mdoc' scrolls inside the sheet's bound, so an element below
    -- the fold is reachable by `n' and invisible without this.
    --
    -- `scrollIntoView' IS LEGITIMATE HERE and forbidden on the table's rows,
    -- which belong to the renderer and its scroller and page, where reaching in
    -- that way would be this page working around an interface it has.  The
    -- document is the SHELL's — its rows, its scroller — and the suite keeps the
    -- distinction by counting: exactly one `scrollIntoView' in the page, this
    -- one, so a second would have to be a reach into something it does not own.
    --
    -- THE BAND IS THE ELEMENT'S OWN MARGIN.  `block:"nearest"' honours
    -- `scroll-margin', so `.de' carrying `scroll-margin-block' is the whole of
    -- the scrolloff: the platform holds the cursor three lines clear of either
    -- edge, in both directions, and stepping down past the band scrolls exactly
    -- far enough to keep it.  An element already inside the band is left where it
    -- is, so the pane never re-centres under a reader walking through it — which
    -- keeps the movement code ONE CALL, no measuring, no `scrollTop' arithmetic,
    -- and the same band whether the pane was last moved by a key or by a wheel.
    -- No easing: the pane is small and a smooth scroll would still be running
    -- when the next `n' lands.
  , "    function keepInView(row) {"
  , "      if (row && typeof row.scrollIntoView === \"function\")"
  , "        row.scrollIntoView({ block: \"nearest\" });"
  , "    }"
    -- A headline line: the four cells side by side, the one under point marked.
    -- A CHILD is the same line indented by its own level, which says the outline
    -- hangs under this entry without drawing a tree.  The state cell takes its
    -- badge hue, so a keyword reads here as it reads in the table.
    -- THE STARS, ORG-CLEANED.  A headline line opens with its own stars, drawn
    -- the way `org-hide-leading-stars' with `org-startup-indented' draws them:
    -- every star but the LAST rendered as a space, so the root reads @* Title@,
    -- a child @ * Title@ and a grandchild @  * Title@.  The indentation IS the
    -- outline, which is why the child lines carry no padding of their own — org's
    -- own arrangement says the depth and a second one would say it twice.
    --
    -- DEPTH IS ORG-INDENT'S OWN ARITHMETIC: TWO spaces a level, so a child's
    -- star sits at column 2 — exactly the column the parent's BODY starts at —
    -- and its own title at 4.  The star is indented to the body level under it,
    -- which makes the outline and the text read as one grid rather than as a list
    -- beside a list.  And it is RELATIVE to the headline the sheet is standing
    -- on: materializing into a child makes THAT line the root, so it reads @* @
    -- and its own children read @  * @, which is what a reader looking at one
    -- entry expects and needs the focus's own level, which the server sends.
    --
    -- IT IS CHROME RATHER THAN A CELL: absent from `r.cells', so `f'/`b' walk
    -- straight past it and `dcol' is an index into the cells alone.  The hidden
    -- stars are SPACES, org's hide face reduced to its effect, so the only ink
    -- left in the prefix is the last star's and it is the page's own.
  , "    const dstars = (level) =>"
  , "      \" \".repeat(Math.max(0, 2 * (level - docLevel()))) + \"* \";"
  , "    const docLevel = () => (editing && editing.level) || 1;"
    -- The cells a headline line actually HAS, which is the whole of what it
    -- draws and the whole of what `f'/`b' stop on: an absent part is not a stop,
    -- so a bare title is one stop and nothing has to be shown before it can be
    -- reached.  `dcol' indexes THIS list rather than the model's four.
  , "    const shown = (r) => (r.cells || []).filter((c) => c.val);"
    -- A paragraph's own text, with the references in it drawn as references.
    -- The element's file range is what the links are intersected against, and it
    -- is the same range `o' scopes by — so what a reader SEES marked in an
    -- element is exactly what `o' there will find.
  , "    function drawPara(row, r) {"
  , "      const at = elementSpan(r);"
  , "      const box = part(row, \"div\", \"dp\");"
  , "      if (at) drawText(box, r.text, at[0], null); else box.textContent = r.text;"
  , "    }"
  , "    function drawCells(row, r, here) {"
  , "      part(row, \"span\", \"ds\", dstars(r.kind === \"child\" ? r.level : docLevel()));"
  , "      shown(r).forEach((c, j) => {"
  , "        const cell = part(row, \"span\", `dc dc-${c.key}`"
  , "          + (here && j === dcol ? \" don\" : \"\"));"
  , "        cell.textContent = c.val;"
        -- The TITLE is the one cell that can hold a reference, and the server
        -- says where it starts (`titleAt') because only it has the sub-span.
        -- A CHILD's title is left as text: its cell is a line of another
        -- entry's outline rather than this document's own bytes, and no offset
        -- for it is sent.
  , "        if (c.key === \"title\" && r.kind === \"head\""
  , "            && editing && typeof editing.titleAt === \"number\")"
  , "          drawText(cell, c.val, editing.titleAt, null);"
  , "        if (c.key === \"state\") cell.style.color = badgeColor(c.val);"
  , "      });"
  , "    }"
    -- MOVEMENT IS TWO AXES, the table's own habit read into the document:
    -- `n'/`p' walk SIBLINGS at the current grain and never dive — a list is ONE
    -- stop however many items it holds, so holding `n' skims the document at
    -- reading grain — and `f'/`b' move the GRAIN itself, finer and broader.
    -- `l'/`h' and the horizontal arrows stay the within-grain cell walk,
    -- walking off either end into the whole-element look rather than bumping,
    -- which is the rule `moveCol' keeps over the table.
  , "    const colStep = (k) => (k === \"<right>\" || k === \"l\" ? 1"
  , "                          : k === \"<left>\" || k === \"h\" ? -1 : 0);"
  , "    const grainStep = (k) => (k === \"f\" ? 1 : k === \"b\" ? -1 : 0);"
  , "    const dcells = (r) => (r && (r.kind === \"head\" || r.kind === \"child\")"
  , "                            ? shown(r).length : 0);"
    -- Siblings at the cursor's grain: a leaf steps only to its owner's other
    -- leaves — they are contiguous behind their whole, so the neighbour row
    -- decides — and the element grain steps over every leaf run whole.  Both
    -- clamp at their ends, silently, the way the old walk clamped at the
    -- document's.
  , "    function docStep(step) {"
  , "      if (!drows.length) return;"
  , "      const cur = drows[dat];"
  , "      let i = dat + step;"
  , "      if (cur && cur.grain === \"leaf\") {"
  , "        const kin = drows[i];"
  , "        if (!kin || kin.grain !== \"leaf\" || kin.owner !== cur.owner)"
  , "          { drawDoc(); return; }"
  , "        dat = i;"
  , "      } else {"
  , "        while (i >= 0 && i < drows.length && drows[i].grain === \"leaf\") i += step;"
  , "        if (i < 0 || i >= drows.length) { drawDoc(); return; }"
  , "        dat = i;"
  , "      }"
  , "      if (!dcells(drows[dat])) dcol = null;"
  , "      dgrain = dcol !== null ? \"cell\""
  , "             : drows[dat].grain === \"leaf\" ? \"leaf\" : \"element\";"
  , "      drawDoc();"
  , "    }"
    -- The grain keys.  `f' on a composite enters its leaves; on a headline it
    -- enters the cells, which ARE that line's finer grain; at the finest it
    -- refuses with an echo — finer that does not exist should say so.  `b' is
    -- the mirror: a cell point broadens to the whole line in one press
    -- (whatever the column — `l'/`h' keep the walk-off spelling), a leaf to
    -- its composite whole, and the element grain is the floor: a no-op with an
    -- echo, never a close, since going OUT of the sheet is `DEL''s and the
    -- movement/context split dies the moment `b' can shut something.
  , "    const leavesOf = (from) => {"
  , "      let n = 0, i = from + 1;"
  , "      while (i < drows.length && drows[i].grain === \"leaf\") { n += 1; i += 1; }"
  , "      return n;"
  , "    };"
  , "    function docFiner(k) {"
  , "      const say = keySaid(k), r = drows[dat];"
  , "      if (!r) return;"
  , "      if (r.grain === \"composite\") {"
  , "        const kin = leavesOf(dat);"
  , "        dat += 1; dgrain = \"leaf\"; drawDoc();"
  , "        say(`grain-finer (${r.name || \"item\"} 1/${kin})`);"
  , "      } else if (dcells(r)) { moveDocCol(k, 1); }"
  , "      else if (r.grain === \"leaf\") say(\"grain-finer (at the finest)\");"
  , "      else say(\"grain-finer (nothing finer here)\");"
  , "    }"
  , "    function docBroader(k) {"
  , "      const say = keySaid(k), r = drows[dat];"
  , "      if (!r) return;"
  , "      if (dcol !== null) {"
  , "        dcol = null; dgrain = \"element\"; drawDoc();"
  , "        say(\"grain-broader (element)\");"
  , "      } else if (r.grain === \"leaf\") {"
  , "        let i = dat;"
  , "        while (i > 0 && drows[i].grain === \"leaf\") i -= 1;"
  , "        dat = i; dgrain = \"element\"; drawDoc();"
  , "        say(`grain-broader (${drows[i].name || drows[i].kind})`);"
  , "      } else say(\"grain-broader (at the element grain)\");"
  , "    }"
  , "    function moveDocCol(k, step) {"
  , "      const say = keySaid(k), n = dcells(drows[dat]);"
  , "      if (!n) { say(\"next-column (no cells in this element)\"); return; }"
  , "      const want = dcol === null ? (step > 0 ? 0 : n - 1) : dcol + step;"
  , "      dcol = want < 0 || want >= n ? null : want;"
  , "      dgrain = dcol === null ? \"element\" : \"cell\";"
  , "      drawDoc();"
      -- The landing is read back off the cursor rather than off WANT, which is
      -- the table's own rule: a column outside the element is no column at all
      -- and the whole-element look is a real move rather than a swallowed key.
  , "      say(`next-column (${dcol === null ? \"element mode\""
      <> " : shown(drows[dat])[dcol].key})`);"
  , "    }"
    -- WHERE AN ELEMENT SITS IN THE FILE, in the offsets `/links' answers in, so
    -- `o' can ask which of the row's links are inside THIS element rather than
    -- matching its text — which a URL the entry spells twice would fool.
    --
    -- Derived from what the answer already carries rather than from a field added
    -- for it.  The BODY is the subtree with the three regions lifted out, and all
    -- three sit ABOVE the paragraphs (a planning line is the line under the
    -- title, both drawers follow it, and the logbook scan stops at the first
    -- child), so every body offset past the title line is displaced by ONE
    -- constant, which is what the two lengths differ by; the title line itself
    -- precedes them and is displaced by nothing.  A CHILD's extent is the
    -- SERVER's and is the one this cannot derive, its own subtree running past
    -- the body the sheet is holding. OFFSETS ARE IN CHARACTERS.  Every span the
    -- server answers in is a CHAR offset into the file (docs/invariants.md), and
    -- JavaScript's `length' and `slice' count UTF-16 units — so ONE astral
    -- character anywhere above a link (an emoji in a title, a rare CJK glyph in a
    -- paragraph) put every element extent and every link segment one out and cut
    -- the link's text in half. Both readings live here, and every offset this
    -- pane computes goes through them.
  , "    const chars = (s) => Array.from(String(s));"
  , "    const clen = (s) => chars(s).length;"
  , "    const cslice = (s, a, b) => chars(s).slice(a, b).join(\"\");"
  , "    const bodyShift = () => clen(editing.org || \"\") - clen(editing.body || \"\");"
  , "    const charOf = (line) =>"
  , "      dlines.slice(0, line).reduce((n, l) => n + clen(l), 0) + line;"
    -- THE ROW'S LINKS, held from the materialize so the DISPLAY can use them.
    -- `/links' is the server's one scan of the subtree and the only authority on
    -- where a link is and what it shows: this page has no bracket grammar and
    -- must not grow one, so what it does with the answer is arithmetic —
    -- intersect the ranges into an element's own coordinates and draw segments.
  , "    let dlinks = [];"
    -- WHICH OF THEM ARE INSIDE A RANGE.  One predicate, two readers: the draw
    -- below and `o', which is where it was written.
  , "    const linksIn = (at, links) => (links || dlinks).filter((l) =>"
  , "      l.span && l.span[0] >= at[0] && l.span[1] <= at[1]);"
    -- TEXT WITH ITS LINKS DRAWN, into INTO.  AT is the file offset TEXT starts
    -- at, which turns a link's file range into an offset in this string.
    --
    -- DISPLAY IS THE DESCRIPTION, SOURCE IS THE FILE — org's own model, and the
    -- table's: `[[T][D]]' shows `D', `[[T]]' shows `T', a bare URL shows itself.
    -- The shown text is the server's `desc' verbatim (`Glance.Query.linkShown'),
    -- so the rule is spelled once, on the side that did the scan.  `RET' opens
    -- the RAW org, brackets and all — the display never becomes the source, so
    -- editing is always over what the file says.  SPAN-DRIVEN, never
    -- search-driven: one URL written three times is three ranges, each drawn
    -- where it stands.
  , "    function drawText(into, text, at, links) {"
  , "      const n = clen(text);"
  , "      let cut = 0;"
  , "      for (const l of linksIn([at, at + n], links)) {"
  , "        const a = l.span[0] - at, b = l.span[1] - at;"
  , "        if (a < cut) continue;"
  , "        if (a > cut) part(into, \"span\", \"dt\", cslice(text, cut, a));"
  , "        part(into, \"span\", \"dl\", l.desc);"
  , "        cut = b;"
  , "      }"
  , "      if (cut === 0) { into.textContent = text; return; }"
  , "      if (cut < n) part(into, \"span\", \"dt\", cslice(text, cut));"
  , "    }"
  , "    function elementSpan(r) {"
  , "      const at = (editing.span || {}).start;"
  , "      if (at === undefined || !r) return null;"
  , "      if (r.kind === \"child\") return r.span ? [r.span.start, r.span.end] : null;"
  , "      if (r.kind === \"head\") return [at, at + charOf(1)];"
  , "      if (r.kind !== \"para\") return null;"
  , "      const shift = at + bodyShift();"
  , "      return [shift + charOf(r.from), shift + charOf(r.to)];"
  , "    }"
    -- `o' IN THE DOCUMENT IS THE TABLE'S `o' AT ONE GRAIN FINER: there it
    -- follows the ROW's links (the whole subtree), here the ELEMENT's.  One
    -- answer either way — `/links' is asked for the row and the element's own
    -- extent is what narrows it — and one gesture: none says so, one opens, and
    -- several raise the popup this page already has, with its own `o' and its
    -- own `RET' edit inside it unchanged.
  , "    function openHere() {"
  , "      const r = drows[dat], b = docBinding(\"org-glance-overview:open\");"
  , "      const at = elementSpan(r);"
  , "      if (!at) { said(b, \"nothing to open here\"); return; }"
  , "      linksOf(editing.id).then((a) => {"
  , "        const links = linksIn(at, a.links || []);"
  , "        followLinks(b, editing.id, { ...a, links }, links);"
  , "      }).catch(failed(b, \"open\"));"
  , "    }"
    -- What the echo and the prompts call the entry the sheet is standing on.
  , "    const docTitle = () =>"
  , "      ((editing && editing.cells && editing.cells.title) || (editing || {}).id || \"\");"
    -- A binding this page can hand `said' and `fire' where no keymap row raised
    -- the write: the document's keys are its own listener's, the way the panel's
    -- and the popups' are, so the command NAME travels with the call.
  , "    const docBinding = (command, seq) => ({ seq: seq || \"RET\", command });"
    -- RET, BY KIND, and that is the whole surface: a child materializes, a
    -- paragraph opens as text, a property and the title open as fields, a
    -- planning row asks for a date, and the state and tag cells raise the page's
    -- own palettes over the row.
  , "    function docEnter() {"
  , "      const r = drows[dat];"
  , "      if (!r) return;"
  , "      if (r.kind === \"child\") { into(r.index); return; }"
  , "      if (r.kind === \"para\") { openEdit(DPARA, r); return; }"
  , "      headEnter(r);"
  , "    }"
    -- The headline's own cells are the ROW's, and a row is what `/command'
    -- addresses: a child headline has no row id, so its cells are read-only here
    -- and the echo says which key reaches the entry that owns them.  Its
    -- planning, its drawer and its body are all still editable, through the lens
    -- that materialized it.
  , "    function headEnter(r) {"
      -- The cell at point, and a cursor can outlive the cells it was taken on: a
      -- stash put back over a headline that has since lost one names a column
      -- that is not there, so the cell is READ rather than assumed.
  , "      const c = dcol === null ? null : shown(r)[dcol];"
  , "      if (!c) { echo(\"RET → no cell selected — f/l picks one\"); return; }"
  , "      if (editing.child !== null) {"
  , "        echo(`RET → a child's ${c.key} is not settable yet — DEL opens its parent`);"
  , "        return;"
  , "      }"
  , "      if (c.key === \"state\") { stateHere(); return; }"
  , "      if (c.key === \"tags\") { tagsHere(); return; }"
  , "      if (c.key === \"title\")"
  , "        { openEdit(DROW, { id: \"CELL:title\", kind: \"cell\", key: \"title\", val: c.val }); return; }"
      -- A RING OF THREE IS PRESSED, NOT PICKED: the two keys answer faster than
      -- any list a cell could raise, so this stays a refusal — one that now
      -- names the keys rather than an absence.
  , "      echo(\"RET → priority cycles on S-<up>/S-<down>\");"
  , "    }"
    -- The state cell raises the value palette this page already has, targeted at
    -- the row the sheet is on rather than at the table's selection: the offer is
    -- `/keywords'' answer for THAT row, so what is shown and what a write takes
    -- are one answer here as everywhere else.
    -- The two element keys are the HEADLINE's, and a CHILD has no row id for a
    -- `/command' to name, so they are refused there the way its cells are.
  , "    function atElement(act) {"
  , "      const r = drows[dat];"
  , "      if (!r || (r.kind !== \"head\" && r.kind !== \"child\"))"
  , "        { echo(\"the headline line takes this — n/p to it\"); return; }"
  , "      if (r.kind === \"child\" || editing.child !== null) {"
  , "        echo(\"a child is not settable yet — DEL opens its parent\");"
  , "        return;"
  , "      }"
  , "      act();"
  , "    }"
    -- The priority ring over the entry the sheet is standing on: one row, so no
    -- grouping is owed — the table's own `cyclePriority' is the general case and
    -- this is it at a set of one, spelled here because the value comes off the
    -- ANSWER's cells rather than off a table row this page may not be showing.
  , "    function cycleHere(step) {"
  , "      const b = docBinding(step > 0 ? \"priority-up\" : \"priority-down\","
  , "                           step > 0 ? \"S-<up>\" : \"S-<down>\");"
  , "      const want = cycled(priorityIn((editing.cells || {}).priority), step);"
  , "      fire(b, \"set-priority\", [editing.id], { priority: want },"
  , "           want ? `[#${want}]` : EMPTY);"
  , "    }"
  , "    const stateHere = () =>"
  , "      docTargets(docBinding(\"org-glance-overview:todo\"), \"set state\", askState);"
    -- And the tag cell raises the tags popup, over the same one row.  It is the
    -- page's `:' with the set settled: a sheet is open on ONE entry, so there is
    -- no marked set to inherit and no question about which rows it means.
  , "    const tagsHere = () =>"
  , "      docTargets(docBinding(\"org-agenda-set-tags\"), \"tags\", askTags);"
    -- DEL IS UP.  In a child the sheet re-materializes the entry above it — the
    -- server's own `parent', null being the row — and lands the cursor back on
    -- the child it came out of.  At the top there is nothing above the row, so
    -- the key is the sheet's door.
    -- A RE-MATERIALIZE of the sheet's own row under CHILD, with K run over the
    -- entry it was standing on and the fresh answer.  Four presses ask for one
    -- (`DEL' up, `RET' into a child, the re-read a commit lands on, and `C-c \''
    -- swapping the shape) and each owes the SAME two guards: the sheet may have
    -- moved on while the read was out, in which case the answer is dropped, and
    -- a read that never came back is the sheet's own `stuck'.
  , "    function reread(child, k) {"
  , "      if (!editing) return;"
  , "      const h = editing;"
  , "      headline(h.id, child).then((fresh) => { if (editing === h) k(h, fresh); })"
  , "        .catch((e) => stuck(subtreeSheet, e.message));"
  , "    }"
  , "    function docUp() {"
  , "      if (!editing) return;"
  , "      if (editing.child === null) { leaveSheet(); return; }"
  , "      const up = editing.parent;"
  , "      reread(up === null ? undefined : up, (h, fresh) => {"
  , "        show(fresh, raw);"
  , "        const back = drows.findIndex((r) => r.kind === \"child\" && r.index === h.child);"
  , "        if (back !== -1) { dat = back; drawDoc(); }"
  , "        echo(`DEL → org-glance-overview:up (${docWhere(fresh)})`);"
  , "      });"
  , "    }"
    -- And RET on a child is DOWN: the sheet re-materializes INTO it, which is the
    -- same route under a `child=' the server handed over.  The subtree the lens
    -- is over moves; the row, the file and the digest do not.
  , "    function into(index) {"
  , "      reread(index, (_h, fresh) => {"
  , "        show(fresh, raw);"
  , "        echo(`RET → org-glance-overview:materialize (${docWhere(fresh)})`);"
  , "      });"
  , "    }"
  , "    const docWhere = (h) => (h.path || []).slice(-1)[0] || h.id;"
    -- WHAT A SUBTREE WRITE ANSWERS, shared by the three that make one: a 200
    -- re-pins the digest and hands the caller its own line, and under that is ONE
    -- ladder — a moved file waits for a keystroke at `conflict', a refused
    -- planning entry names its field through `stuck', a request that never landed
    -- says why.  It reports whether the write LANDED, which is what the sheet's
    -- own flush resolves to and what decides a re-read here.
  , "    function landed(h, onOk) {"
  , "      return (a) => {"
  , "        if (a.status === 200) {"
  , "          h.digest = a.body.digest;"
  , "          sync(\"synced\");"
  , "          onOk(a);"
  , "          return true;"
  , "        }"
    -- A refused planning entry is a 409 like a moved file, and it waits for a
    -- keystroke the same way — but it names the field rather than the file, so
    -- it goes through `stuck' and says so.
  , "        if (a.status === 409 && a.body.reason !== \"planning\") sync(\"conflict\");"
  , "        else stuck(subtreeSheet, a.body.error || `sync failed (${a.status})`);"
  , "        return false;"
  , "      };"
  , "    }"
    -- THE COMMIT, and every element of the SUBTREE goes through it: one `POST
    -- /headline' carrying the body, the drawer and the planning line as the model
    -- holds them, pinned to the digest this sheet was handed.  DROP names the
    -- paragraphs a deletion is taking out, those leaving no trace in the model to
    -- read afterwards.  The answer re-pins the digest and the sheet
    -- re-materializes off it, so the model is the server's reading of what was
    -- just written rather than this page's guess at it.
  , "    const commitDoc = (what, drop) =>"
  , "      commitDocWith(bodyText(drop), () => { if (what) echo(`RET → ${what}`); });"
    -- The sheet re-read, in place: the same entry, the fresh parts, the cursor
    -- kept by id.  It is what a commit lands on and what a socket frame naming
    -- this row asks for — the watch is the channel a `/command' write comes back
    -- through, exactly as it is for the table.
  , "    function reload() {"
  , "      if (!editing) return;"
  , "      reread(editing.child, (_h, fresh) => {"
  , "        editing = fresh;"
  , "        fill(fresh);"
  , "        sync(\"synced\");"
  , "      });"
  , "    }"
    -- THE EDIT OVERLAY, ONE mechanism over four surfaces.  The renderer owns its
    -- rows and rewrites them as it scrolls, so an edit cannot live inside one:
    -- the fields sit OVER the table, anchored to the row the cursor is on.  The
    -- document opens an element's key and value, or a paragraph as text; the
    -- tags popup opens one cell as a field over itself; the link popup opens
    -- two.  Everything else is the same — the class that shows the box, the
    -- anchor, the blur on the way out — so a SHAPE says what differs (`DROW',
    -- `DPARA', `TROW', `LROW') and this holds the gesture.
    --
    -- SNAPSHOTTED AT OPEN, the property this shape exists to have.  No key can
    -- move the cursor while a row is open, but a MOUSE CLICK can, and a commit
    -- that re-read the cursor would write the text typed for one row into
    -- whichever row the reader landed on.  `edit' keeps what was opened over and
    -- a commit is handed it, so every surface has the guard the tags rename was
    -- written with.
    --
    -- One `edit' for all four, no two being up at once: the document needs the
    -- subtree sheet open, and each popup is raised over the table alone and
    -- counts as `typing()' while it stands, so neither can raise the other.
    -- `dediting()', `dparaing()', `renaming()' and `lediting()' ask WHOSE it is.
  , "    let edit = null;"
  , "    function openEdit(o, row) {"
  , "      edit = { o, row };"
  , "      el(o.box).className = \"on\";"
  , "      o.fill(row);"
      -- The renderer stamps `tv-sel' on its own frame, so a row selected in
      -- THIS tick has no marked element yet: `+' would measure the row the
      -- cursor was on before it.  One frame later there is one.
  , "      soon(placeEdit);"
  , "      o.focus(row);"
  , "    }"
    -- SHUT MINE, and O is which surface is asking.  The two shapes share one
    -- `edit', and a caller naming its own keeps the sharing from reaching across:
    -- the tags popup CAN stand over an open materialize sheet — clicking the
    -- sheet's own chrome blurs its textarea, `typing()' goes false and every
    -- `table' row is live again, the same hole `openSettings' refuses by hand —
    -- so an unscoped shut would let the sheet's own `fill' and `shut' silently
    -- cancel an open tag rename.  Naming the shape restores exactly the isolation
    -- the two hand-written shutters had, for one argument. TAB INSIDE AN OPEN
    -- EDIT hops that edit's own fields, every shape declaring them already
    -- (`fields'), so the hop reads the list rather than naming a pair — a third
    -- field then works everywhere instead of leaving three copies silently wrong.
    -- It WRAPS, which makes S-TAB the same line, and a focus outside the list
    -- lands on the first, where the two hand-written pairs put it.
  , "    function hop() {"
  , "      const ids = edit.o.fields;"
  , "      const at = ids.findIndex((id) => el(id) === document.activeElement);"
  , "      el(ids[(at + 1) % ids.length]).focus();"
  , "    }"
  , "    function shutEdit(o) {"
  , "      if (!edit || edit.o !== o) return;"
  , "      el(edit.o.box).className = \"\";"
  , "      for (const id of edit.o.fields) el(id).blur();"
  , "      edit = null;"
  , "    }"
    -- ESC OVER AN OPEN EDIT, wherever it is open: the overlay goes and the thing
    -- under it stands, holding the text it was opened on.  WHAT names it in the
    -- pill — an element, a row, a link, a tag — and the SHAPES are the caller's
    -- own, the scoping `shutEdit' asks for; the document names two, having a
    -- second box for a paragraph.  One sentence, so a fifth surface cannot word
    -- the same event differently.
  , "    const cancelEdit = (what, ...shapes) => {"
  , "      for (const o of shapes) shutEdit(o);"
  , "      echo(`ESC → keyboard-quit (${what} unchanged)`);"
  , "    };"
    -- Where the overlay sits: over the row the renderer has selected.  Its
    -- GEOMETRY is the only thing this page reads out of a mount's own DOM, and it
    -- reads nothing about the row but where it is.
    --
    -- A `cells' shape narrows to a RUN of columns as well, and it names them BY
    -- KEY — the tags popup edits `["title"]', the link popup `["title", "url"]'
    -- (the derived type column being the one it may not open), and the document's
    -- two name none and take the whole element.  The keys are resolved against
    -- the shape's OWN column list, the one the SERVER declared
    -- (`Glance.Query.linkColumns', `tagColumns') and this page embeds, so
    -- reordering those columns moves the overlay with them and inserting one
    -- ahead of the run costs nothing.  A key no column carries resolves to
    -- nothing and the placement is a no-op — a box left where it was rather than
    -- a box over the wrong cells.  The GUTTER `flags: true' puts in front is
    -- skipped by the class the renderer already stamps, so the resolution counts
    -- the popup's own columns and nothing of its chrome. WHERE THE ROW IS, and a
    -- shape says how to find it.  Three of the four surfaces are table-view
    -- mounts and read the renderer's own selected row through the handle's
    -- published root — the mount publishes it, so the one geometry read this page
    -- makes goes through a published door — and the document is no mount at all,
    -- so it names the element under point directly. One reader either way, so the
    -- geometry, the cell run and the resize stay one implementation.
  , "    const anchorOf = (o) => {"
  , "      if (o.anchor) return o.anchor();"
  , "      const m = o.mount();"
  , "      return m ? m.el.querySelector(\"tbody tr.tv-sel\") : null;"
  , "    };"
  , "    function placeEdit() {"
  , "      if (!edit) return;"
  , "      const o = edit.o;"
  , "      const tr = anchorOf(o);"
      -- A page with no layout — the suite's, and a sheet still `display:none'
      -- while it is being filled — measures nothing and leaves the overlay
      -- exactly where it was put.
  , "      if (!tr || typeof tr.getBoundingClientRect !== \"function\") return;"
  , "      const span = o.cells && cellSpan(o.cells, o.cols);"
  , "      if (o.cells && !span) return;"
  , "      const tds = span && [...tr.querySelectorAll(\"td:not(.tv-box)\")];"
  , "      const from = tds && tds[span[0]], to = tds && tds[span[1]];"
  , "      if (o.cells && !(from && to)) return;"
  , "      const pane = el(o.pane);"
  , "      if (typeof pane.getBoundingClientRect !== \"function\") return;"
  , "      const a = tr.getBoundingClientRect();"
  , "      const b = pane.getBoundingClientRect();"
  , "      const s = el(o.box).style;"
      -- FROM THE PANE'S PADDING BOX, AND WITH ITS SCROLL.  An absolutely
      -- positioned child is placed against its containing block's PADDING box and
      -- scrolls with the content rather than with the viewport, so a pane
      -- carrying a border or a scroll offset needs both back: `clientTop' is that
      -- border, `scrollTop' that offset.  `#mprops' has neither, which is why the
      -- bare delta was right where this was written and wrong the moment `#mdoc'
      -- — bordered, padded and scrolling — reused it.
  , "      s.top = `${a.top - b.top - pane.clientTop + pane.scrollTop}px`;"
  , "      s.height = `${a.height}px`;"
  , "      if (!o.cells) return;"
  , "      const l = from.getBoundingClientRect(), rt = to.getBoundingClientRect();"
  , "      s.left = `${l.left - b.left}px`;"
  , "      s.width = `${rt.right - l.left}px`;"
  , "    }"
    -- WHERE A RUN OF NAMED COLUMNS SITS: the leftmost and rightmost of KEYS as
    -- indices into COLS, or null where any of them names no column there.  The
    -- RUN IS THE COLUMNS' ORDER rather than the shape's, a box being drawn from
    -- one edge to the other and a shape spelling its keys the other way round
    -- meaning the same two cells.  Pure and order-only, so the answer is a
    -- property of the two lists and of nothing else on the page, which lets the
    -- suite check it against the server's own column declaration rather than
    -- against a copy of it.  A declaration rather than a `const', so a direct
    -- `eval' of this glue leaks it the way it leaks `whichKeys'.
  , "    function cellSpan(keys, cols) {"
  , "      const at = (keys || []).map((k) => (cols || []).findIndex((c) => c.key === k));"
  , "      if (!at.length || at.some((i) => i < 0)) return null;"
  , "      return [Math.min(...at), Math.max(...at)];"
  , "    }"
    -- The overlay is anchored to a row's box, so the window resizing has to move
    -- it — once, here, for every surface, since one `placeEdit' answers for
    -- whichever is open.  A mount's own scrolling is registered with it.
  , "    window.addEventListener(\"resize\", placeEdit);"
  , "    el(\"mdoc\").addEventListener(\"scroll\", placeEdit, true);"
    -- THE EDIT SHAPES, both on the page's own overlay mechanism (`openEdit'), so
    -- a document element, a panel row, a tag and a link are edited alike — the
    -- snapshot at open, the blur on the way out, ESC through the keymap's
    -- `cancel'.  What differs is the box: a pair of fields for a key and a value,
    -- and a textarea for a paragraph, which is text and wants its newlines.
    -- `anchor' is the one thing a shape declares here that a mount's does not:
    -- the document is no mount, so it names the element under point rather than
    -- the renderer's `tv-sel' row.
  , "    const docElAt = () => dcursor;"
    -- A KEY-AND-VALUE SHAPE, and there are two of them: the document's property
    -- row and the panel's.  P prefixes the pair of fields (`dkey'/`dval',
    -- `pkey'/`pval'), LOCKED says whose key org owns rather than the author —
    -- the document's title cell, the panel's three planning rows — and REST is
    -- the one thing they differ in beyond that, where the anchor comes from.
    -- The key is read-only exactly where it is locked, and the focus opens on
    -- the VALUE unless there is no key to have yet, which is the add row.
  , "    const pairShape = (box, pane, p, locked, rest) => Object.assign({"
  , "      box, pane, fields: [`${p}key`, `${p}val`],"
  , "      fill: (r) => {"
  , "        el(`${p}key`).value = r.key;"
  , "        el(`${p}val`).value = r.val;"
  , "        el(`${p}key`).readOnly = locked(r);"
  , "      },"
  , "      focus: (r) => (locked(r) || r.key ? el(`${p}val`) : el(`${p}key`)).focus(),"
  , "    }, rest);"
  , "    const DROW = pairShape(\"dedit\", \"mdoc\", \"d\", (r) => r.kind === \"cell\","
  , "      { mount: () => null, anchor: docElAt });"
  , "    const DPARA = {"
  , "      box: \"dpara\", pane: \"mdoc\", fields: [\"dtext\"],"
  , "      mount: () => null, anchor: docElAt,"
  , "      fill: (r) => { el(\"dtext\").value = r.text; },"
  , "      focus: () => el(\"dtext\").focus(),"
  , "    };"
  , "    const dediting = () => !!edit && edit.o === DROW;"
  , "    const dparaing = () => !!edit && edit.o === DPARA;"
  , "    const docOpen = () => dediting() || dparaing();"
    -- The surface is UP whenever a subtree sheet is on screen, in EITHER shape.
    -- The structured document holds the keys with NOTHING focused, the way the
    -- panel's nav does, so `typing()' has to count it; RAW MODE counts with it,
    -- a textarea being BLURRABLE — clicking the sheet's own header does it — and
    -- a surface that stopped counting the moment a reader touched its chrome left
    -- every `table' row live under an open sheet, `d' among them, which archives
    -- the row behind it.  WHAT THAT COSTS IS `q': it is scope `table', so with
    -- either sheet open it is dead and `quitWindow''s `editing ? leaveSheet()'
    -- arm is unreachable, the sheet's doors being ESC and the backdrop.  Over the
    -- table `q' says there is no window to quit, now the whole of what it says.
  , "    const docHolds = () => editing !== null;"
    -- The commit: the ELEMENT takes the text its fields are holding, and the
    -- write goes out.  The element is the one the overlay OPENED over, never the
    -- one point is on now — the snapshot `openEdit' keeps, and what a mouse click
    -- under an open edit would otherwise redirect.
    -- B is the binding that fired, since TWO keys commit an open element —
    -- `C-x C-s' and org's `C-c C-c' — and the echo names the command that ran.
    -- Absent, the caller is `RET' inside the overlay, which spells its own line.
  , "    function commitDocEdit(b) {"
  , "      const spoke = (what) => (b ? said(b, what) : echo(`RET → ${what}`));"
  , "      if (!edit) return;"
  , "      const r = edit.row;"
  , "      if (edit.o === DPARA) {"
  , "        const text = el(\"dtext\").value;"
  , "        shutEdit(DPARA);"
  , "        if (text === r.text) { spoke(\"paragraph unchanged\"); return; }"
  , "        r.text = text;"
  , "        commitDoc(\"paragraph written\");"
  , "        return;"
  , "      }"
  , "      const key = el(\"dkey\").value, val = el(\"dval\").value;"
  , "      shutEdit(DROW);"
  , "      if (r.kind === \"cell\") { retitle(val); return; }"
  , "      if (r.key === key && r.val === val) { spoke(\"property unchanged\"); return; }"
  , "      r.key = key; r.val = val;"
  , "      commitDoc(key.trim() ? `:${key.trim()}: written` : \"property dropped\");"
  , "    }"
    -- The title is a CELL, so it is a command rather than a subtree write: the
    -- span math replaces the title's own characters and the keyword in front of
    -- it and the tags behind it keep their bytes.  The refusals — an empty title,
    -- a second line — are the server's, and they are the whole request's.
  , "    function retitle(val) {"
  , "      fire(docBinding(\"org-glance-overview:rename\"), \"set-title\", [editing.id],"
  , "           { title: val }, `retitled ${JSON.stringify(val.trim())}`);"
  , "    }"
  , "    const cancelDocEdit = () => cancelEdit(\"element\", DROW, DPARA);"
    -- The sheet is ONE surface with two panes, so it is one entry in `SURFACES'
    -- and ESC puts back whichever pane's edit is open.  Below that the ladder
    -- falls through to the sheet itself, which is where it always did.
  , "    const sheetOpen = () => docOpen() || pediting();"
  , "    const cancelSheetEdit = () => (pediting() ? cancelRow() : cancelDocEdit());"
    -- DELETION IS KIND-AWARE, and over this pane the kind it reaches is the
    -- PARAGRAPH: the table's own gesture, over the document's own flags — `d'
    -- flags, a second `d' — or `D' — takes every flagged element, `u' takes a
    -- flag off.  A paragraph is spliced OUT of the body; a HEADLINE, this entry's
    -- own line or a child's, is REFUSED and says so, deleting an entry being
    -- neither what this sheet is for nor something a command backs.  The drawer
    -- and the planning line are the PANEL's, and `pdelete' is their own half of
    -- the same gesture.  One write for the set, however many blocks it names.
  , "    function ddelete(ids, how) {"
  , "      const gone = new Set(ids);"
  , "      const named = drows.filter((r) => gone.has(r.id));"
  , "      const taken = named.filter((r) => r.kind === \"para\");"
  , "      if (named.length !== taken.length)"
  , "        append(\"sync\", \"warn\","
  , "               \"a headline is not deleted from the sheet — this writes elements only\");"
  , "      if (!taken.length) { echo(`D → org-delete-element (${how(0)})`); return; }"
  , "      const body = bodyText(new Set(taken.map((r) => r.id)));"
  , "      commitDocWith(body,"
  , "        () => echo(`D → org-delete-element (${how(taken.length)} taken)`));"
  , "    }"
    -- The one write the document makes, and BODY is the caller's because a
    -- deletion cannot rebuild it out of the model — the paragraphs it took out
    -- are still in it.  SAY is the line the 200 earns, and the sheet re-reads on
    -- it, which is what makes the model the server's reading of what was written
    -- rather than this page's guess at it.
  , "    function commitDocWith(body, say) {"
  , "      if (!editing) return;"
  , "      const h = editing;"
  , "      sync(\"syncing\");"
  , "      post(h.id, h.digest, { body, properties: props(), planning: planning() },"
  , "           null, h.child)"
  , "        .then(outcome)"
  , "        .then((a) => { if (editing === h && landed(h, say)(a)) reload(); })"
  , "        .catch((e) => stuck(subtreeSheet, e.message));"
  , "    }"
    -- The property panel is a table-view MOUNT, the sheet's RIGHT pane, beside
    -- the structured document.  The renderer is this page's list widget: a
    -- drawer is a list of RECORDS — a key and a value, one shape per row — so it
    -- draws the drawer, where the document beside it is a list of KINDS and is
    -- this page's own.  That buys no rows of its own to style, no cursor of its
    -- own to move and no second answer to what a flagged row looks like.
    --
    -- MODEL AND VIEW.  `prows' is the model — a key, a value, and whether org
    -- owns the key — and the mount is a view of it, re-set on every change.  A
    -- row HOLDS its COMMITTED text; the open row's two fields are the edit in
    -- progress and nothing else reads them, which makes a commit the only thing
    -- that can make the sheet dirty.  The cursor, the flags and the scrolling are
    -- the renderer's, so none of them is kept here.  It stays MODAL, dired's
    -- shape rather than a form's: in NAV nothing is focusable, leaving the plain
    -- letters free to be movement, and RET is what puts fields on screen.
    --
    -- The planning entries are rows of this same list — three FIXED ones, in
    -- org's own order, ahead of the drawer's properties.  Fixed means the key is
    -- org's rather than the author's: RET opens the value alone, an empty value
    -- is the entry absent, and a delete CLEARS the entry where it would drop a
    -- property.  A row's ID is stable for the life of the sheet — the planning
    -- key, or `P' and a number handed out once — so a flag, a selection and a
    -- deletion all name the same row after any number of edits above it.
    --
    -- The identity property is in neither pane: `ORG_GLANCE_ID' is the row id
    -- the table keys its updates off, and the server keeps it out of what it
    -- hands over and puts it back verbatim afterwards
    -- ('Glance.Query.hiddenProperties').  There is nothing here to warn about
    -- and nothing to filter — and nothing rowed is nothing flaggable.
  , "    const PLANNING = " <> jsonValue planningKeywords <> ";"
  , "    const PCOLS = [ { key: \"key\", header: \"Key\" },"
  , "                    { key: \"value\", header: \"Value\" } ];"
  , "    let pmount = null, prows = [], pseq = 0;"
    -- A MOUNT THIS PAGE KEEPS, made on the first ask and handed back afterwards:
    -- a mount per raise would leave a theme listener behind every time the
    -- reader opened a sheet or followed a row.  PANE is the scroller the edit
    -- overlay is anchored inside — caught in the CAPTURE phase, which reaches it
    -- without this page naming the element that scrolls; the window resizing is
    -- the other half and is registered once, with `placeEdit'.  Three surfaces
    -- mount this way and differ only in their host, their columns and their
    -- options.
  , "    function mountOnce(host, cols, opts, pane) {"
  , "      const m = TableView.mount(el(host), { columns: cols, rows: [] }, opts);"
  , "      el(pane).addEventListener(\"scroll\", placeEdit, true);"
  , "      return m;"
  , "    }"
  , "    function mounted() {"
  , "      if (pmount) return pmount;"
  , "      pmount = mountOnce(\"mptable\", PCOLS, {"
        -- No bar and no resident filter: five rows of a drawer are not something
        -- a reader narrows, and the overlay this leaves behind is never raised.
  , "        palette: true,"
        -- Flags alone: the gutter carries the flag's edge, no checkbox is
        -- drawn, and nothing here reads a mark.
  , "        flags: true,"
        -- The key line under the table already names every key, once.
  , "        actionHints: false,"
  , "        flagHelp: \"d/D delete · u unflag\","
  , "      }, \"mprops\");"
  , "      return pmount;"
  , "    }"
  , "    const prowsOf = () =>"
  , "      prows.map((r) => ({ id: r.id, cells: { key: r.key, value: r.val } }));"
    -- Every change to the model ends here.  AT is the row to land the cursor on
    -- and is left out where it should stay where it is.
  , "    function repaint(at) {"
  , "      const m = mounted();"
  , "      m.setRows(prowsOf());"
  , "      if (at) m.select(at);"
  , "    }"
  , "    function drawProps(list, plan) {"
  , "      mounted();"
  , "      prows = []; pseq = 0;"
  , "      shutEdit(PROW);"
  , "      el(\"mprops\").className = \"\";   // and the panel gives the keys back"
  , "      const held = new Map(plan || []);"
  , "      for (const key of PLANNING)"
  , "        prows.push({ id: `PLN:${key}`, key, val: held.get(key) || \"\", fixed: true });"
  , "      for (const p of list)"
  , "        prows.push({ id: `P${pseq++}`, key: p[0], val: p[1], fixed: false });"
      -- A different drawer: these flags were about the last one.  `setRows'
      -- deliberately keeps them, so taking them off is this page's to ask for.
  , "      pmount.clearFlags();"
  , "      repaint(prows[0].id);"
  , "    }"
    -- Where the cursor is, in the model's terms.  The renderer's answer is the
    -- one that decides; this page keeps no copy of it.
  , "    const patAt = () => prows.findIndex((r) => r.id === selectedId(pmount));"
    -- The add affordance, and the whole of it: `+' puts an empty property at the
    -- end of the drawer and opens it.  Keyboard-first means the KEY is the offer,
    -- where a row that is always empty was chrome every reader of the panel had
    -- to filter back out.  A row whose key is emptied is still a property
    -- deleted, which is what `d' spells as a key press.
  , "    function addProperty() {"
  , "      const id = `P${pseq++}`;"
  , "      prows.push({ id, key: \"\", val: \"\", fixed: false });"
  , "      repaint(id);"
  , "      openRow();"
  , "    }"
    -- What the panel would write: every property row carrying a key, in the
    -- order they sit in.  A row whose key has been emptied is a deletion.  Both
    -- fields are trimmed, because the server hands them over trimmed: what the
    -- panel can show is then exactly what it can write, and a space nobody could
    -- ever see again cannot be typed into a file.
  , "    const props = () => prows"
  , "      .filter((r) => !r.fixed)"
  , "      .map((r) => [r.key.trim(), r.val.trim()])"
  , "      .filter((p) => p[0] !== \"\");"
    -- And the planning line: the fixed rows carrying a value, in org's order.
    -- An empty row is that entry absent, so clearing all three is how the line
    -- comes off — the server drops it rather than writing a bare keyword.
  , "    const planning = () => prows"
  , "      .filter((r) => r.fixed && r.val.trim() !== \"\")"
  , "      .map((r) => [r.key, r.val.trim()]);"
    -- Crossing the panes, and the two modes.  NEITHER pane focuses anything in
    -- the structured shape: the document holds the keys on the left and the panel
    -- on the right, both with nothing focused, which leaves every printable key
    -- free to be movement and a command.  `pnav' says which of the two has them;
    -- `typing()' counts the whole sheet as a focus of its own (`docHolds'), so
    -- the table's keys stay dead under either.  Raw mode is the exception, a
    -- textarea focusing itself.
  , "    const pnav = () => el(\"mprops\").className === \"on\";"
  , "    function enterPanel() {"
  , "      el(\"mprops\").className = \"on\"; el(\"mdoc\").className = \"\";"
  , "      el(\"mtext\").blur();"
  , "    }"
  , "    function leavePanel() {"
  , "      el(\"mprops\").className = \"\"; el(\"mdoc\").className = \"on\";"
  , "    }"

    -- THE PANEL'S SHAPE: the document's, one pane over, since both are two
    -- fields over the whole row.  A planning row's key is ORG's rather than the
    -- author's, so its field is read-only text with a caret in it — which is
    -- what `pairShape' calls locked, and what sends the focus to the value.
  , "    const PROW = pairShape(\"pedit\", \"mprops\", \"p\", (r) => r.fixed,"
  , "      { mount: () => pmount });"
  , "    const pediting = () => !!edit && edit.o === PROW;"
  , "    function openRow() {"
  , "      const at = patAt();"
  , "      if (at !== -1) openEdit(PROW, prows[at]);"
  , "    }"
    -- Committing: the row takes the text the fields are holding and the overlay
    -- goes.  This is the one thing that can make the sheet dirty from the panel
    -- — an edit nobody committed was never in `props()'.  A fixed row keeps its
    -- key, which is org's rather than the author's.  The row is the one the
    -- overlay OPENED over, never the one the cursor is on now.
  , "    function commitRow() {"
  , "      const r = edit.row;"
  , "      if (!r.fixed) r.key = el(\"pkey\").value;"
  , "      r.val = el(\"pval\").value;"
  , "      shutEdit(PROW);"
  , "      repaint();"
  , "    }"
    -- ESC over an open row is the ROW's: the overlay goes and the text the row
    -- is holding stands, which is the text it was opened on.  The sheet's own
    -- ESC ladder therefore only ever sees the key from nav — that is why this
    -- runs from the keymap's `cancel' rather than from a listener of its own.
  , "    const cancelRow = () => cancelEdit(\"row\", PROW);"
    -- DELETION IS THE TABLE'S GESTURE, over the renderer's own flags: `d' flags
    -- the row at point, `d' again — or `D' — takes every flagged row, and `u'
    -- takes a flag off.  One implementation of the gesture in this page, the set,
    -- the wash and the count all being the mount's.
    --
    -- WHAT "taken" MEANS is the row's.  A property is dropped, the emptied key
    -- spelled as a key press.  A planning entry is CLEARED and its row stands:
    -- the three are org's keys rather than the author's, and an empty value is
    -- already how an entry is absent.
    -- IDS is the set the key worked out, HOW the word the pill calls it: a caller
    -- that has already found the row and read the flags does not make this look
    -- for them again.  HOW is a function of what LANDED, and this deletion is
    -- local and total, so it is asked about the whole set.
  , "    function pdelete(ids, how) {"
  , "      const gone = new Set(ids);"
  , "      const cleared = prows.filter((r) => gone.has(r.id) && r.fixed);"
  , "      for (const r of cleared) r.val = \"\";"
  , "      prows = prows.filter((r) => r.fixed || !gone.has(r.id));"
  , "      repaint();"
      -- The command name is the BINDING's and the brackets carry what it did:
      -- org has no one function for taking a planning entry off — it is
      -- `org-schedule' or `org-deadline' under a prefix — so the line names the
      -- keys it cleared rather than claiming a property function did it.
  , "      const also = cleared.map((r) => r.key).join(\", \");"
  , "      echo(`D → org-delete-property (${how(ids.length)}"
      <> "${also ? ` · ${also} cleared` : \"\"})`);"
  , "    }"
    -- THE SHEET'S OWN KEYS, over BOTH panes, and the ONE private listener that
    -- registers AHEAD of the dispatch — written with the sheet, near the top of
    -- the glue — so it sees a key first.
    --
    -- WHY A PRIVATE LISTENER IS SAFE, said here once for all of them: a surface
    -- holding the keys makes `typing()' true, which kills every `table' row, so
    -- the only map row that can fire around one of these is `ESC' — which is the
    -- one that should, a key this does not claim falling through untouched.  The
    -- listeners BEHIND the dispatch take it from the other side; this one takes
    -- it from in front, and stands down under a `momentary()'.  FOUR STATES,
    -- tried in the order a key belongs to and only ever one true: an open PANEL
    -- row, an open DOCUMENT element, the panel in nav, and the document, which is
    -- where the sheet opens.
    --
    -- TAB CROSSES THE PANES, out of the document into the panel's cursor and back
    -- out of nav into the document, each cursor where it was left.  Two stops, so
    -- both directions are one toggle and S-TAB is the same line.  Inside an OPEN
    -- row TAB hops that row's two fields, suspending the crossing while one is
    -- open; raw mode has one pane and nowhere to cross to, so TAB is the
    -- browser's there.  IN THE DOCUMENT the movement is the table's letters
    -- exactly: `n'/`p', `j'/`k' and the vertical arrows walk the elements,
    -- `f'/`b', `l'/`h' and the horizontal ones the cells of the element that has
    -- any; RET dispatches by kind, DEL is UP, and `d'/`D'/`u' are the deletion
    -- gesture over the paragraphs.  In the PANEL it is the same movement over the
    -- drawer, `RET' opens a row, `+' adds one and the same gesture deletes.  With
    -- a PARAGRAPH open the keys are the textarea's own — a paragraph is text and
    -- RET a newline in it — so the commit is `C-x C-s', the keymap's
    -- `save-buffer' over whichever edit is open.
    --
    -- AUTO-REPEAT IS MOVEMENT'S, and this listener owes the rule itself: running
    -- AHEAD of the dispatch and claiming what it takes, the map's own `ONCE' list
    -- can never reach a key of this one's.  A held `n' crosses the pane and a
    -- held TAB is a crossing either way; every key that WRITES delivers exactly
    -- one press — a held `d' would flag and delete from one, the confirmation the
    -- two-press shape exists to be, and a held `S-<up>' was one `/command' per
    -- repeat off a cell the answer before it had already moved, a burst of 409s
    -- from a single press.
  , "    document.addEventListener(\"keydown\", (e) => {"
      -- THE SHEET STANDS DOWN UNDER A MOMENTARY.  It is the workspace, and a
      -- palette or a popup raised over it — from the table or from the document
      -- itself — holds the keys until it dissolves.  This listener registers
      -- FIRST, so without the guard it would claim the very letter the palette
      -- was raised to read.
  , "      if (!editing || raw || momentary()) return;"
  , "      const k = keyName(e), crossing = k === \"TAB\" || k === \"S-TAB\";"
  , "      if (!k) return;"
  , "      if (dparaing()) return;   // the textarea's; C-x C-s commits and ESC restores"
  , "      const once = (act) => { if (!e.repeat) act(); };"
  , "      if (pediting()) {"
  , "        if (crossing) hop();"
  , "        else if (k === \"RET\") once(commitRow);"
  , "        else return;   // ESC is the keymap's, and puts the row back"
  , "      } else if (dediting()) {"
  , "        if (crossing) hop();"
  , "        else if (k === \"RET\") once(commitDocEdit);"
  , "        else return;   // ESC is the keymap's, and puts the element back"
  , "      } else if (pnav()) {"
  , "        if (crossing) leavePanel();"
  , "        else if (k === \"RET\") once(openRow);"
  , "        else if (k === \"+\") addProperty();"
  , "        else if (rowStep(k)) stepIn(pmount, rowStep(k));"
  , "        else if (!flagPress(k, e, PFLAGS)) return;"
  , "      } else if (crossing) enterPanel();"
  , "      else {"
  , "        const step = rowStep(k), side = colStep(k), depth = grainStep(k);"
  , "        if (step) docStep(step);"
  , "        else if (depth > 0) docFiner(k);"
  , "        else if (depth < 0) docBroader(k);"
  , "        else if (side) moveDocCol(k, side);"
  , "        else if (k === \"RET\") once(docEnter);"
  , "        else if (k === \"DEL\") once(docUp);"
      -- The table's own keys, over the entry the sheet is standing on: a
      -- priority is a cell of the headline line, so the ring is the same ring
      -- and the command is the same command.  Refused on a child for the cells'
      -- own reason.
  , "        else if (k === \"S-<up>\" || k === \"S-<down>\")"
  , "          once(() => atElement(() => cycleHere(k === \"S-<up>\" ? 1 : -1)));"
      -- `o' at one grain finer than the table's: the ELEMENT's links rather than
      -- the row's.
  , "        else if (k === \"o\" || k === \"!\") once(openHere);"
      -- The two keys that SET a part rather than edit one, and they work at the
      -- ELEMENT: an absent state or an absent tag is no cell to walk onto, so
      -- the question is asked of the headline the sheet is standing on and never
      -- of a column point.  They are the table's own keys, over one row.
  , "        else if (k === \"t\") once(() => atElement(stateHere));"
  , "        else if (k === \":\") once(() => atElement(tagsHere));"
  , "        else if (!flagPress(k, e, DFLAGS)) return;"
  , "      }"
  , "      e.preventDefault();"
  , "    });"
    -- DIRED'S `d', ONE implementation over THREE surfaces — the table, this panel
    -- and the tags popup.  The first press flags the row at point; a second `d'
    -- on an already-flagged row IS `D' — it calls the same handler, so it takes
    -- EVERY flagged row rather than the one under it; `u' takes a flag off and
    -- walks on.  The flag is the confirmation, so there is no prompt, and a lone
    -- flag is a set of one, which leaves the single-row flow unchanged.
    --
    -- THE CURSOR IS ASKED FOR FIRST AND THE FLAGS SECOND, which is what each
    -- branch needs: `D' means "take these" and a lone row is a set of one, so it
    -- lands on a mount whose renderer never had flags, while the two presses
    -- that MOVE a flag are exactly the ones an asset predating them cannot serve
    -- and are what the refusal is for.
    --
    -- A SHAPE says what differs: a mount, where the cursor is, what "take these"
    -- means, what the surface LOGS when a flag moves, and FOUR PHRASES — the
    -- line for a mount with no flags, the line for an empty cursor, and the line
    -- each of the two flagging presses earns.  The feature detection, the
    -- two-press rule, the set-or-row choice and the walk after `u' are the
    -- gesture, and the gesture is here; a third surface joins by naming those.
    --
    -- SAY is the caller's rather than the shape's, because WHO IS SPEAKING is:
    -- the popups say `KEY → phrase' out of a listener with no binding in its
    -- hand, and the table says it through `said', which spells the binding's own
    -- command name and puts the phrase in brackets.  So a phrase is the whole
    -- line on one surface and the bracket on another, and each surface's `say'
    -- and its phrases travel together.
    --
    -- HOW words the count for the pill, a FUNCTION of what LANDED rather than of
    -- what was asked for: the popups' takes are local and total, so they call it
    -- with the size of the set, where the table's is a write that can come back
    -- partly refused and a name over the asked-for count would read as a whole
    -- answer.
  , "    function flagKey(k, s, say) {"
  , "      const m = s.mount();"
  , "      const at = s.at();"
  , "      if (at === null) { say(s.none); return; }"
  , "      const flags = flagsOn(m) ? m.getFlagged() : [];"
  , "      if (k === \"D\" || (k === \"d\" && flags.indexOf(at) !== -1)) {"
  , "        const ids = flags.length ? flags : [at];"
      -- The flags are SPENT before the take, on every surface: a mount keeps a
      -- flag whose row is hidden — which is what makes a flag outlive the
      -- repaint the take causes — so a set left standing would be taken again by
      -- the next press and the row at point would never be reachable again.
  , "        if (can(m, \"clearFlags\")) m.clearFlags();"
  , "        s.take(ids, flags.length ? (n) => `${n} flagged` : (n) => (n ? \"row\" : n));"
  , "        return;"
  , "      }"
  , "      if (!flagsOn(m)) { say(s.missing); return; }"
  , "      if (k === \"u\") {"
  , "        m.unflagRow(at);"
  , "        s.note(at, false);"
  , "        say(s.unflag);"
  , "        s.walk();"
  , "        return;"
  , "      }"
  , "      m.flagRow(at);"
  , "      s.note(at, true);"
  , "      say(s.flag);"
  , "    }"
    -- The popups have nothing to log: their rows are a property and a tag, which
    -- the echo already names, where the table's are org headlines the strip
    -- reports one line per.  So the hook is theirs to leave empty rather than a
    -- branch inside the gesture.
  , "    const unlogged = () => {};"
    -- The panel's phrases, and its cursor as an ID: `patAt' answers with an
    -- INDEX, which is the panel's own currency and nothing the gesture reads.
  , "    const PFLAGS = {"
  , "      mount: () => pmount, take: pdelete, note: unlogged,"
  , "      walk: () => stepIn(pmount, 1),"
  , "      missing: \"this table-view.js has no delete flags\","
  , "      none: \"org-delete-property (no row)\","
  , "      unflag: \"delete-unflag (flag cleared)\","
  , "      flag: \"delete-flag (d again deletes)\","
  , "      at: () => { const i = patAt(); return i === -1 ? null : prows[i].id; },"
  , "    };"
    -- And the document's, whose `mount' is no renderer's — four calls over a Set
    -- of element ids — which makes it a fourth surface of the SAME gesture rather
    -- than a second implementation: `flagKey' asks a mount for four things and
    -- never what kind of mount it is.  `missing' is therefore unreachable here
    -- and is still spelled, a shape leaving it out being one field short of the
    -- three beside it.
  , "    const DFLAGS = {"
  , "      mount: () => dmount, take: ddelete, note: unlogged,"
  , "      walk: () => docStep(1),"
  , "      missing: \"this document has no flags\","
  , "      none: \"org-delete-element (no element)\","
  , "      unflag: \"delete-unflag (flag cleared)\","
  , "      flag: \"delete-flag (d again deletes)\","
  , "      at: () => (drows[dat] ? drows[dat].id : null),"
  , "    };"
    -- How a surface with no binding in its hand speaks: the key, the arrow, and
    -- the phrase whole.
  , "    const keySaid = (k) => (what) => echo(`${k} → ${what}`);"
    -- THE GESTURE'S THREE KEYS AS ONE PRESS, over whichever SHAPE the surface
    -- declares, and false for a key that is not one of them so a caller's chain
    -- goes on past it.  The HELD-key guard is here rather than on each surface:
    -- `ONCE' governs dispatch rows and these three live in listeners the
    -- dispatch does not own, so a repeat that survived would flag a row and take
    -- it in ONE press — which is the confirmation the two-press shape exists to
    -- be.
  , "    const flagPress = (k, e, shape) => {"
  , "      if (k !== \"d\" && k !== \"D\" && k !== \"u\") return false;"
  , "      if (!e.repeat) flagKey(k, shape, keySaid(k));"
  , "      return true;"
  , "    };"
    -- What a flush sends: the subtree whole in raw mode, the two panes apart
    -- otherwise.  The server joins them, so this page never spells a drawer.
  , "    const asked = () => raw"
  , "      ? { org: el(\"mtext\").value }"
  , "      : { body: bodyText(), properties: props(), planning: planning() };"
    -- ONE BUTTONLESS SHEET, twice over.  The subtree sheet and the settings
    -- sheet are the same flow over different files — a state word, a flush, and
    -- a close that syncs on the way out — so the ladder is written once and each
    -- sheet supplies the verbs it differs in: `dirty', `flush', `refresh' (the
    -- digests a conflict overwrites under), `shut', and the log `scope' its own
    -- lines are filed under.  Never both up at once, `openSettings' refusing over
    -- an open sheet, which is what makes `activeSheet' total.  Where a sheet
    -- stands is ONE word and `note' is its only writer: the header wears it as
    -- text and as a class, and everything that asks reads it back off the sheet.
    -- With no buttons the keys are the whole of the offer, so the states that
    -- wait for one say which key — and the retry line is spelled once, three
    -- copies of it being three chances to drift.
  , "    const RETRY = \" — C-x C-s retry · ESC discard\";"
  , "    const WORDS = { synced: \"synced\", syncing: \"syncing…\","
  , "      conflict: \"conflict — C-x C-s overwrite · ESC discard\","
  , "      error: \"error\" + RETRY };"
  , "    function note(s, next, message) {"
  , "      s.state = next;"
  , "      el(s.noteId).className = next;"
  , "      el(s.noteId).textContent = message || WORDS[next];"
  , "    }"
  , "    const stuck = (s, why) => note(s, \"error\", why && `${why}${RETRY}`);"
  , "    const subtreeSheet = {"
  , "      noteId: \"mnote\", scope: \"sync\", state: \"synced\","
  , "      closed: \"closed without writing — the file is as it was\","
  , "      dirty: () => dirty(),"
  , "      flush: () => flush(editing.digest),"
    -- What a conflict overwrites under: the digest the file carries NOW, unless
    -- the sheet moved on to another headline while the read was out.
  , "      refresh: () => {"
  , "        const h = editing;"
  , "        return headline(h.id, h.child).then((b) => {"
  , "          if (editing !== h) return false;"
  , "          h.digest = b.digest;"
  , "          return true;"
  , "        });"
  , "      },"
  , "      shut: () => shut(),"
  , "    };"
  , "    const activeSheet = () => (editing ? subtreeSheet : settings ? configSheet : null);"
  , "    const sync = (next, message) => note(subtreeSheet, next, message);"
  , "    function shut() {"
  , "      el(\"modal\").className = \"\"; editing = null; base = \"\"; baseProps = null;"
  , "      shutEdit(DROW); shutEdit(DPARA); shutEdit(PROW);"
  , "      drows = []; dlines = []; dflags.clear(); dcursor = null;"
  , "      el(\"dlist\").textContent = \"\";"
  , "      el(\"mprops\").className = \"\"; el(\"mdoc\").className = \"\";"
  , "    }"
  , "    // POST the sheet over the subtree it is standing on, pinned to DIGEST —"
  , "    // the ROW's extent where it never left the row, and the entry's under a"
  , "    // `child='.  A 200 carries the file's new digest, so the receipt chains"
  , "    // and the next flush needs no re-materialize."
  , "    function flush(digest) {"
  , "      const h = editing, sent = asked();"
  , "      sync(\"syncing\");"
  , "      return post(h.id, digest, sent, null, h.child)"
  , "        .then(outcome)"
  , "        .then(landed(h, () => {"
  , "          base = raw ? sent.org : base;"
  , "          baseProps = raw ? null : JSON.stringify([sent.properties, sent.planning]);"
  , "        }))"
  , "        .catch((e) => { stuck(subtreeSheet, e.message); return false; });"
  , "    }"
  , "    // C-x C-s, over whichever sheet is up.  Mid-edit it is a manual flush;"
  , "    // on a conflict it is the deliberate keystroke that overwrites — ask for"
  , "    // the digests the files carry now and post what the author is looking at"
  , "    // over them."
  , "    function saveSheet(b) {"
      -- COMMIT THE OPEN EDIT.  The structured document has no ladder of its own,
      -- so `save-buffer' here is what the design calls it: the alias for
      -- committing the element that is open, and the only commit a PARAGRAPH has,
      -- RET being a newline inside one.  With nothing open the key falls through
      -- to the sheet's own flush, which in raw mode is the whole ladder and in
      -- the structured mode writes the document as the model holds it.
  , "      if (docOpen()) { commitDocEdit(b); return; }"
  , "      const s = activeSheet();"
  , "      if (!s || s.state === \"syncing\") return;"
  , "      if (s.state !== \"conflict\") { s.flush(); return; }"
  , "      s.refresh().then((ok) => ok && s.flush()).catch((e) => stuck(s, e.message));"
  , "    }"
  , "    // The way out — ESC, the backdrop, q.  Pristine costs no request and"
  , "    // touches no file; dirty flushes and closes on the 200; a sheet with"
  , "    // trouble in it discards, which is what a second ESC is."
  , "    function leaveSheet() {"
  , "      const s = activeSheet();"
  , "      if (!s) return;"
  , "      if (s.state === \"conflict\" || s.state === \"error\") {"
  , "        s.shut();"
  , "        append(s.scope, \"info\", s.closed);"
  , "        return;"
  , "      }"
  , "      if (!s.dirty()) { s.shut(); return; }"
  , "      if (s.state !== \"syncing\") s.flush().then((ok) => ok && s.shut());"
  , "    }"
    -- The backdrop is the mouse's ESC, for both sheets: a click that lands on
    -- the veil itself rather than on the box over it.
  , "    for (const id of [\"modal\", \"config\"])"
  , "      el(id).addEventListener(\"click\","
  , "        (e) => { if (e.target === el(id)) leaveSheet(); });"
    -- And for the two MOMENTARY veils, backdrops of the same family that had
    -- none: a click landed on them and nothing happened, where the same click on
    -- a sheet closed it.  What it does differs because the surfaces do — a sheet
    -- leaves through its own ladder, pristine costing no request, and a momentary
    -- is answered and gone — so the two loops are two rules rather than one with
    -- a branch in it.
  , "    for (const [id, off] of [[\"links\", shutLinks], [\"tags\", shutTags]])"
  , "      el(id).addEventListener(\"click\","
  , "        (e) => { if (e.target === el(id)) off(); });"
    -- C-c ' — org's `edit-special' rhyme, one subtree seen two ways: body and
    -- panel, or the raw org the panes were cut out of.  The cut is the server's,
    -- so the toggle RE-READS the headline rather than splitting or joining
    -- anything here, which keeps an org parser out of this page.  A re-read
    -- cannot carry unsaved work, so a dirty sheet is refused and told which key
    -- would let it through; being a fresh materialize, it lands at `synced'
    -- whatever it was at before.
  , "    function toggleRaw(b) {"
  , "      if (!editing) return;"
  , "      if (dirty()) { said(b, \"sync first — C-x C-s\"); return; }"
  , "      const want = !raw;"
  , "      reread(editing.child, (_h, fresh) => {"
  , "        editing = fresh; raw = want;"
  , "        fill(fresh);"
  , "        sync(\"synced\");"
  , "        if (raw) el(\"mtext\").focus(); else el(\"mtext\").blur();"
  , "        said(b, raw ? \"raw org\" : \"structured document\");"
  , "      });"
  , "    }"
  , "    // A tab closing on an edited sheet still owes the file the text:"
  , "    // `keepalive' outlives the document, and a pristine sheet sends nothing."
  , "    addEventListener(\"beforeunload\", () => {"
  , "      if (!dirty()) return;"
  , "      post(editing.id, editing.digest, asked(), { keepalive: true }, editing.child)"
  , "        .catch(() => {});"
  , "    });"
  , ""
  , "    // Rows.  The renderer virtualizes, so a row outside the window has no"
  , "    // element: movement is ids out of `getVisible()' handed to `select(id)'."
  , "    // Which row is on is the renderer's too — it answers with the column,"
  , "    // and a click moves both without telling us — so the DOM read is the"
  , "    // fallback for an asset predating the call, and nothing is kept here."
  , "    const visible = () => (table ? table.getVisible() : []);"
  , "    const focusedId = () => {"
  , "      if (cells()) return table.getSelection().id;"
  , "      const tr = document.querySelector(\"#app .tv-table tbody tr.tv-sel\");"
  , "      return tr ? tr.dataset.id : null;"
  , "    };"
  , "    function pick(list, i) {"
  , "      if (!list.length) { append(\"cmd\", \"info\", \"no rows to move through\"); return; }"
  , "      const id = list[Math.max(0, Math.min(list.length - 1, i))].id;"
  , "      // Row movement carries the column along: null until a horizontal key"
  , "      // picks one, so a page nobody has moved sideways in keeps whole rows."
  , "      table.select(id, column());"
  , "    }"
    -- A row step is the renderer's `selectStep': it carries the column, and it
    -- turns the page at either end, which only the renderer knows there is —
    -- `getVisible()' is one page's worth, so index arithmetic here would stop
    -- dead at a boundary.  An asset predating the call has no pages either, so
    -- the old walk over the visible ids is exactly right for it.
  , "    const steps = () => can(table, \"selectStep\");"
  , "    function move(step) {"
  , "      if (steps()) {"
  , "        if (visible().length) table.selectStep(step);"
  , "        else append(\"cmd\", \"info\", \"no rows to move through\");"
  , "        return;"
  , "      }"
  , "      const list = visible(), at = list.findIndex((r) => r.id === focusedId());"
  , "      pick(list, at === -1 ? (step > 0 ? 0 : list.length - 1) : at + step);"
  , "    }"
    -- What a key says when it has run: the sequence, the COMMAND, and what
    -- happened in brackets after it.  The command is the blob's own identifier,
    -- spoken verbatim — `> → last-row', never `> → last row' — these names being
    -- the handle a rebinding config will address a function by, and a reader who
    -- learns one off the echo has to be able to type it.  The prose goes in the
    -- brackets, naming an outcome rather than a function.  Every key echoes
    -- through here, so there is one shape and one place the rule can be broken.
  , "    const said = (b, what) =>"
  , "      echo(`${b.seq} → ${b.command}${what ? ` (${what})` : \"\"}`);"
    -- Pages.  The turn is the renderer's, and the bracket says where it landed
    -- rather than repeating the key: `] → next-page (page 3/129)' reads the
    -- same at a stop as at a turn.
  , "    const pager = () => can(table, \"nextPage\") && can(table, \"pageInfo\");"
    -- WHICH page is showing, 1 for an asset with no pages: `visible()' is one
    -- page's worth, so anything asking what the view still holds has to know
    -- which page it asked about.
  , "    const pageNow = () => (pager() ? table.pageInfo().page : 1);"
    -- The sort, which is `^''s alone now: an ORDER IS A QUERY, so the agenda
    -- states its own by carrying a `sort:' token and no page here calls `sortBy'.
    -- Named with the rest of the optional calls, this being where a reader greps
    -- for which renderer calls are feature-detected.  `sortPromote' composes the
    -- chain and WRITES IT INTO THE QUERY as ONE arrow-form `sort:' token
    -- (`sort:state->title:desc'), which comes back through `onFilter' like any
    -- other query change — so the URL carries the order, DEL takes a key off it,
    -- and the server is asked for the order it is about to be sent.  This page
    -- keeps no record of the chain: the handle publishes it (getSort) and the
    -- query spells it.
  , "    const sorts = () => can(table, \"sortPromote\");"
  , "    function turnPage(b, step) {"
  , "      if (!pager()) { said(b, \"this table-view.js has no pager\"); return; }"
  , "      if (step > 0) table.nextPage(); else table.previousPage();"
  , "      const at = table.pageInfo();"
  , "      said(b, `page ${at.page}/${at.pages}`);"
  , "    }"
    -- The ends of the buffer, progressively.  `<' takes the page's first row;
    -- pressed AGAIN, already on it, it turns back a page and lands on THAT page's
    -- first row, and `>' mirrors it — so the pair reaches the ends of the SET
    -- rather than of the page, and a reader who wants one page turned still has
    -- the brackets.  Page one's first row and the last page's last row are stops:
    -- the turn declines and nothing moves.
    --
    -- Both climbs land at the wrong end and need a select of their own, the
    -- renderer putting the cursor on the end it ARRIVES at — `nextPage' on the
    -- new page's first row, `previousPage' on its last — the opposite end from
    -- the one the key is named for, in both directions.  The column comes back
    -- out of the renderer: a turn re-selects with the column it had, so reading
    -- `column()' after one reads what it kept.
    --
    -- A turn is an explicit page action, so the renderer snaps out of continuous
    -- presentation back to paged at the page it turned to — which is what a key
    -- named for an end of the buffer means, the reader having asked for a
    -- boundary and paged being the presentation that has them.
  , "    function endStop(b, last) {"
  , "      const list = visible();"
  , "      if (!list.length) { append(\"cmd\", \"info\", \"no rows to move through\"); return; }"
  , "      const end = (rows) => rows[last ? rows.length - 1 : 0].id;"
    -- Not there yet — or an asset with no pages, where there is nowhere to
    -- climb to and the within-page jump is the whole of the key.
  , "      if (!pager() || focusedId() !== end(list)) {"
  , "        table.select(end(list), column());"
  , "        said(b, \"\");"
  , "        return;"
  , "      }"
  , "      if (!(last ? table.nextPage() : table.previousPage())) { said(b, \"\"); return; }"
  , "      const turned = visible();"
  , "      if (turned.length) table.select(end(turned), column());"
  , "      const at = table.pageInfo();"
  , "      said(b, `page ${at.page}/${at.pages}`);"
  , "    }"
  , "    // Cells.  The column is part of the renderer's selection, so it needs no"
  , "    // state here: it rides along with row"
  , "    // movement, and goes when the selection that holds it goes.  A whole-row"
  , "    // selection has none, and the first horizontal key lands on the first"
  , "    // column whichever direction asked."
  , "    const cells = () => can(table, \"getSelection\");"
  , "    const column = () => (cells() ? table.getSelection().col : null);"
  , "    function moveCol(b, step) {"
  , "      if (!cells()) { said(b, \"this table-view.js has no cell selection\"); return; }"
  , "      const at = column(), want = at === null ? 0 : at + step;"
      -- Walking off the cells LANDS rather than bumping: a column index outside
      -- the table is no column at all to the renderer, which nulls it and gives
      -- back the whole-row look, so the step is handed over out of range and the
      -- exit is a real move — where a clamp here used to swallow the key and say
      -- `at last' at a wall the renderer does not have.  The column comes back
      -- out of `column()' rather than off `want', the renderer's answer deciding.
  , "      const id = focusedId();"
  , "      if (!id || !table.select(id, want)) { said(b, \"no row\"); return; }"
  , "      const now = column();"
  , "      said(b, now === null ? \"row mode\" : (cols[now].header || cols[now].key));"
  , "    }"
    -- Marks.  The renderer holds them, keyed by id, so nothing is kept here:
    -- which rows are marked, how many there are and what a mark survives are all
    -- its answers.  Dired's advance is this page's — the key that marks is the
    -- key that walks, which makes a held `m' a run down a column.
  , "    const marking = () => can(table, \"toggleMark\");"
    -- Archive flags are the renderer's for the same reason marks are: a flag has
    -- to outlive a `setRows', a filter hiding its row and a page it is not on,
    -- and only the thing that draws the rows can do that.  An asset predating
    -- the calls says so rather than growing a shell-side set a paint would lose.
  , "    const flagging = () => flagsOn(table);"
  , "    const isFlagged = (id) => flagging() && table.getFlagged().indexOf(id) !== -1;"
      -- The same question of the other set, and asked the same way: the renderer
      -- is consulted at the moment it matters rather than copied into a set here.
  , "    const isMarked = (id) => marking() && table.getMarked().indexOf(id) !== -1;"
    -- The log names a row the way the table does: by its title, out of the rows
    -- in hand — the page on screen, and the unfiltered baseline behind it.  A row
    -- in neither is named by its id, a lookup failure a reader can still act on.
    -- `displayText' is the renderer's own link rule, so what the line spells is
    -- what the cell shows.
    -- The row ID names, out of the two lists this page has in hand — the page on
    -- screen, and the unfiltered baseline behind it — or an empty one, so a
    -- caller reads a cell off the answer rather than guarding the lookup.
  , "    const rowOf = (id) => visible().concat(all).find((r) => r.id === id) || {};"
  , "    const titleOf = (id) => {"
  , "      const cell = (rowOf(id).cells || {}).title;"
  , "      const shown = typeof TableView.displayText === \"function\""
  , "        ? TableView.displayText(cell) : String(cell || \"\");"
  , "      return shown || id;"
  , "    };"
    -- One wording for every write a key makes: the pill counts what landed, the
    -- log says which rows they were.  Bulk is one line per row, since a set
    -- spanning three files can come back two-thirds applied.
  , "    const noted = (id, what) =>"
  , "      append(\"cmd\", \"info\", `headline ${JSON.stringify(titleOf(id))} ${what}`);"
    -- TOGGLING is `m', which flips the way dired's does and takes the renderer's
    -- word for where it landed.  `u' is never a toggle: it flips too, then puts
    -- back anything it just laid down, so walking a column of marks clears it
    -- rather than laying it again.  Both calls are one statement apart and the
    -- renderer coalesces its painting to a frame, so the flip is never drawn.
  , "    function mark(b, toggling) {"
  , "      if (!marking()) { said(b, \"this table-view.js has no marks\"); return; }"
  , "      const id = focusedId();"
  , "      if (!id) { said(b, \"no row\"); return; }"
    -- `u' takes the archive FLAG off first: it is the more recent thing a reader
    -- put on the row and the one that would otherwise write a file.  One key for
    -- both, which is what dired does, and the echo says which.  THE ASYMMETRY IS
    -- THE TABLE'S and stays here: over the two popups `u' is the flag key and
    -- nothing else, where over the table it is the MARK key preferring a flag
    -- when the row is wearing one — so the clearing, the log line and the walk
    -- belong to the shared gesture (`flagKey' does all three) and the choice to
    -- hand it the key belongs to this surface.
  , "      if (!toggling && isFlagged(id))"
  , "        { flagKey(\"u\", XFLAGS(b), (what) => said(b, what)); return; }"
  , "      let on = table.toggleMark(id);"
  , "      if (on && !toggling) on = table.toggleMark(id);"
  , "      said(b, `${on ? \"marked\" : \"unmarked\"} · ${table.markedCount()}`);"
  , "      move(1);"
  , "    }"
    -- Commands.  A structured write names ROWS and lets the server compute the
    -- spans, so nothing here knows what a headline looks like; `edit-link' names
    -- a RANGE and knows no more for it, the range coming out of `GET /links' and
    -- going back as it came.  Nothing here touches the table afterwards either:
    -- the rows arrive over the socket once the watch has re-read the files, the
    -- way an editor's save arrives.
    --
    -- Which rows a command runs over is per COMMAND, and the two answers are
    -- deliberately different.  `set-state' takes the MARKED set, the generic bulk
    -- selection — mark a run of rows, set them all.  Archiving takes the FLAGGED
    -- set, a selection made for archiving and nothing else (`flagged' below): the
    -- destructive-looking command must not inherit a selection a reader built for
    -- some other purpose.  Either way the set is the renderer's and is asked for
    -- when the command runs rather than tracked here.
  , "    const targets = () => {"
  , "      const marked = marking() ? table.getMarked() : [];"
  , "      if (marked.length) return marked;"
  , "      const id = focusedId();"
  , "      return id ? [id] : [];"
  , "    };"
    -- A partial answer is ordinary here: each file is its own write, so one that
    -- moved on disk refuses its rows while the rest land.  The count goes in the
    -- pill and every refusal in the log. HOW names what the pill says inside the
    -- parentheses, and is given the number of rows that LANDED so a partial
    -- answer cannot read as a whole one: the count alone is the default, and a
    -- key that ran over a named set says which set it was, falling back to the
    -- bare count when nothing landed, since "row" over zero rows would be a lie.
    -- The route, and the only place this page spells it: a body in, the answer
    -- out, and the server's own words thrown where it refused.  Both writing keys
    -- go through it — the one that names rows and the one that makes one — so
    -- what a refusal looks like is decided once.
  , "    const postCommand = (body) => postJSON(\"/command\", body).then(unwrap);"
    -- And the one shape a failed write takes: the pill says what went wrong and
    -- the strip keeps it, named by the command that was asked for.
  , "    const failed = (b, name) => (e) => {"
  , "      said(b, e.message);"
  , "      append(\"cmd\", \"error\", `${name} failed: ${e.message}`);"
  , "    };"
    -- And the shape a palette raised over an unanswered request takes: a
    -- palette with nothing in it is no offer, so the overlay comes down and the
    -- reason goes to the strip.  It takes the prompt it was raised FOR, since a
    -- reader who left and raised another must not have that one closed.
  , "    const askFailed = (mine, name) => (e) => {"
  , "      if (prompting === mine) unask();"
  , "      append(\"cmd\", \"error\", `${name} failed: ${e.message}`);"
  , "    };"
    -- The results come back out, undefined where the request failed: a caller
    -- with state of its own to fold them into reads them, every other one
    -- ignoring the answer, which is the pill and the log this already wrote.
    -- The tags popup's three flows fold them into the tag sets it is drawing and
    -- `archive' spends the marks they landed on, each guarding the undefined the
    -- same way, since a failed write landed nothing.
    --
    -- WHAT a command means to the rows it touched is the CALLER's, so nothing
    -- here branches on the name past the wording below: a per-name arm in this
    -- shared path is one every future command has to be read against.
    --
    -- PIN is the optimistic lock, and the caller's, because knowing what a write
    -- was measured against is: `edit-link' holds char offsets into a file and
    -- sends the digest that file had when `/links' measured them, where the
    -- commands naming a PROPERTY of a row — a keyword, a tag — are measured
    -- against nothing and send none.  Absent, the route still refuses a file that
    -- moved on DISK; the pin adds refusing one the STORE has re-read since.
    -- What one landed write did, per row.  The names are the route's whole
    -- vocabulary, so the wording is a TABLE beside them rather than a ladder
    -- inside the shared path — one entry per name, the way `HANDLERS' is one
    -- entry per command, and a name added to the route is a line here.  The
    -- fallback is `set-state', whose phrase reads off the keyword it set;
    -- `edit-link' is the one whose pill line IS the line the log wants.
  , "    const VERBED = {"
  , "      \"edit-link\": (args, verb) => verb,"
  , "      \"set-title\": (args) => `retitled ${JSON.stringify(args.title)}`,"
  , "      \"set-priority\": (args) =>"
  , "        (args.priority ? `priority [#${args.priority}]` : \"priority cleared\"),"
  , "      archive: () => \"archived\","
  , "      \"add-tag\": (args) => `tagged :${args.tag}:`,"
  , "      \"remove-tag\": (args) => `untagged :${args.tag}:`,"
  , "      \"rename-tag\": (args) => `retagged ${args.from}→${args.to}`,"
  , "      \"set-planning\": (args) =>"
  , "        `${args.keyword.toLowerCase()} ${args.date || \"cleared\"}`,"
  , "    };"
  , "    const stated = (args) => (args.keyword ? `→ ${args.keyword}` : \"state cleared\");"
  , "    const verbed = (name, args, verb) => (VERBED[name] || stated)(args, verb);"
  , "    function fire(b, name, ids, args, verb, how, pin) {"
  , "      return postCommand({ name, ids, args, digests: pin }).then((answer) => {"
  , "        const results = answer.results || [];"
  , "        const bad = results.filter((x) => !x.ok);"
  , "        const landed = results.length - bad.length;"
  , "        said(b, `${verb} · ${how ? how(landed) : landed}`);"
  , "        const what = verbed(name, args, verb);"
  , "        for (const x of results) if (x.ok) noted(x.id, what);"
  , "        if (bad.length)"
  , "          append(\"cmd\", \"error\", bad.map((x) => `${x.id}: ${x.error}`).join(\" · \"));"
  , "        return results;"
  , "      }).catch(failed(b, name));"
  , "    }"
    -- AN ARCHIVED ROW SPENDS ITS MARK, the way it spends its flag: the marks the
    -- rows RESULTS landed on were carrying, taken off.  The mark is the
    -- renderer's and survives a `setRows' and a filter that hides its row — which
    -- is what makes it useful, and what would otherwise leave an archived row
    -- marked INVISIBLY: `markedCount()' would count it, `M' and `U' would answer
    -- about it, and it would come back marked the moment a reader looked at
    -- `tag:*archive*'.  Only the rows that LANDED, a refused one not having been
    -- archived, and none at all where the request itself failed, which is the
    -- undefined `fire' hands a `.then' after its own `catch'.  `toggleMark' is
    -- the only door the renderer offers, so a membership test comes first, and it
    -- is `isMarked' — the renderer asked at the moment it matters, never a set
    -- kept here — which also feature-detects, so an asset with no marks has none
    -- to spend.
  , "    function unmark(results) {"
  , "      for (const x of results || [])"
  , "        if (x.ok && isMarked(x.id)) table.toggleMark(x.id);"
  , "    }"
    -- WHERE POINT GOES AFTER AN ARCHIVE: THE NEXT SURVIVING ROW.  Worked out
    -- from POINT rather than from the set — down the page for the first row not
    -- leaving, and only failing that back UP it for the nearest one — since what
    -- a reader is owed is the row that takes the place of the one they were
    -- standing on, and a scattered set says nothing about that.  Nothing at all
    -- where every row on the page is leaving: an empty view has nowhere to land.
    --
    -- The UP half always agrees with the renderer's own keeping, which clamps to
    -- the last surviving row — nothing survives below point, so point is past
    -- every survivor and the place it stood clamps to the same row.  Nothing can
    -- exercise it alone; it is here so the rule is one sentence resting on no
    -- other component for half of itself.
    --
    -- Taken HERE, at fire time, the answer being unrecoverable once the rows have
    -- gone: the gap they left is exactly what a later read cannot see, which is
    -- why the renderer's own keeping falls back to the visual PLACE and can land
    -- past a run of rows that went from under point.
    --
    -- WHETHER anything is owed is decided elsewhere.  This says where point WOULD
    -- go; `settled' fires only once the row it was on has left the view, and
    -- `spent' drops the whole thing when the answer says that row was not
    -- archived — so an archive over a set point is not IN owes nothing and costs
    -- nothing, without a third statement of the same rule at this end.  `at' is
    -- the anchor's place among the rows that SURVIVE, where it will be sitting
    -- once they go — the fallback for the anchor itself vanishing between the
    -- fire and the landing.
  , "    function anchorFor(ids) {"
  , "      const rows = visible(), going = (id) => ids.indexOf(id) !== -1;"
  , "      const from = focusedId();"
  , "      const here = from ? rows.findIndex((r) => r.id === from) : -1;"
  , "      if (here === -1) return null;"
      -- The PAGE it was taken on.  `visible()' is one page, so "the row point
      -- was on has left the view" is only answerable about the page it was on:
      -- a reader who turned a page between the write and its watch event would
      -- otherwise be told every row of it had gone.
  , "      const on = pageNow();"
  , "      let want = null;"
  , "      for (let i = here + 1; want === null && i < rows.length; i += 1)"
  , "        if (!going(rows[i].id)) want = rows[i];"
  , "      for (let i = here - 1; want === null && i >= 0; i -= 1)"
  , "        if (!going(rows[i].id)) want = rows[i];"
  , "      if (want === null) return null;"
  , "      return { from, on, id: want.id,"
  , "               at: rows.filter((r) => !going(r.id)).indexOf(want) };"
  , "    }"
    -- And the landing, run at every door the archive's rows can reach the view
    -- through: the filtered REFETCH, the one they actually leave by; the
    -- unfiltered SPLICE, which for an archive re-sends the row rather than
    -- removing it (the tag moved, the headline did not) but is what a row leaving
    -- over the socket would come through; and the repaint a reconnect costs, the
    -- same rows arriving by a third road.
    --
    -- IT IS ALWAYS SPENT, and lands only where there is something to land: the
    -- anchor describes ONE watch step and must not outlive it, or a page turn and
    -- somebody else's edit later would pull the cursor to a row this write had an
    -- opinion about long ago.  Nothing to land in two cases — the row point was
    -- standing on is still there (an unfiltered client keeps it, and so does a
    -- `tag:*archive*' query that still matches it), and the page showing is not
    -- the page the anchor was taken on, where `visible()' can say nothing about
    -- whether that row is still in the view.
  , "    function settled() {"
  , "      arrived();"
  , "      const want = leaving;"
  , "      leaving = null;"
  , "      if (!want || !table) return;"
  , "      if (pageNow() !== want.on) return;"
  , "      if (visible().some((r) => r.id === want.from)) return;"
  , "      land({ id: want.id, col: column() }, want.at);"
  , "    }"
    -- The capture's landing, at those same doors and spent the same way, and
    -- `land''s ordinary rule asked only where there is something to land ON: a
    -- filter that hides the new row, a page it is not on, or a watch step that
    -- has not delivered it yet all leave point exactly where it stands.  Asking
    -- unguarded would pull the cursor to row one, since `land' falls through to
    -- an index and there is no honest index to fall to here.
  , "    function arrived() {"
  , "      const want = arriving;"
  , "      arriving = null;"
  , "      if (!want || !table) return;"
  , "      if (visible().some((r) => r.id === want)) land({ id: want, col: column() });"
  , "    }"
    -- Archiving: the tag goes on, the headline stays, and the default view stops
    -- showing it.  WHICH ROWS is `flagKey''s — the FLAGGED set when there is one
    -- and the row at point otherwise — and never the marked one: a mark is the
    -- generic bulk selection a reader lays down to set a state over a run of
    -- rows, and letting the archive key inherit it makes every mark a loaded
    -- gun.  So the table names no set of its own here; it hands the gesture a
    -- key and takes back the ids.
    --
    -- The marks are spent HERE rather than in `fire': what an archived row owes
    -- its mark is the archive gesture's rule, and a name test in the shared path
    -- would be one every command added after it has to be read against.
    --
    -- `fire' catches its own request failures and resolves to `undefined', so the
    -- tail this hangs off it needs a catch of its OWN or a throw inside the
    -- spending would be an unhandled rejection where the old in-`fire' placement
    -- wrote a `cmd error' line.  It is reachable: `marking()' feature-detects
    -- `toggleMark' alone while `isMarked' also calls `getMarked', so an asset
    -- carrying one and not the other throws here. A refused write moved no row,
    -- so the landing it armed is dropped with the marks it did not spend — the
    -- rows are all still there and point is still on one of them.  Both are what
    -- the ANSWER says rather than what the request asked for, which is why they
    -- are folded in one place.  MINE is the anchor this answer is about, compared
    -- rather than assumed: two archives can be out at once, and an earlier answer
    -- naming none of the later one's rows would otherwise disarm it.  The anchor
    -- is decided BEFORE the marks, `unmark' being able to throw on an asset
    -- carrying half the mark calls, which would leave a landed write still armed.
  , "    const spent = (mine) => (results) => {"
  , "      if (mine && leaving === mine"
  , "          && !(results || []).some((x) => x.ok && x.id === mine.from))"
  , "        leaving = null;"
  , "      unmark(results);"
  , "    };"
    -- WHAT THE TABLE'S SECOND PRESS TAKES: the ids `flagKey' worked out, archived
    -- in one request under the binding that asked.  The anchor is taken HERE, at
    -- fire time, while the view still holds the rows about to go; `fire' notes
    -- the landed rows one line each; and HOW is the gesture's, a function of the
    -- LANDED count so a partly refused write cannot read as a whole one.
  , "    function archive(b, ids, how) {"
  , "      leaving = anchorFor(ids);"
  , "      fire(b, \"archive\", ids, {}, \"archived\", how)"
  , "        .then(spent(leaving)).catch(failed(b, \"archive\"));"
  , "    }"
    -- THE TABLE'S SHAPE, and the third `flagKey' surface.  A function of the
    -- BINDING, everything it says and everything it fires being spoken through
    -- one — `said' spells the binding's own command name, so `d' reads
    -- `archive-flag' and `D' reads `org-glance-overview:delete' out of the same
    -- gesture, which is what the two keys mean.  Its phrases are BRACKET CONTENTS
    -- where the popups' are whole lines, `said' supplying the arrow and the
    -- command name; its `note' is the one that has anything to log, a table row
    -- being an org headline and the strip naming every one a key touches.
  , "    const XFLAGS = (b) => ({"
  , "      mount: () => table, at: focusedId, walk: () => move(1),"
  , "      take: (ids, how) => archive(b, ids, how),"
  , "      note: (id, on) =>"
  , "        noted(id, on ? \"marked for deletion\" : \"unmarked for deletion\"),"
  , "      missing: \"this table-view.js has no archive flags\","
  , "      none: \"no row\","
  , "      unflag: \"flag cleared\","
  , "      flag: \"flagged — d again archives\","
  , "    });"
    -- ORG'S PRIORITY RING, and the wrap is THROUGH NONE: up runs
    -- @none → C → B → A → none@ and down the reverse.  That is org's own cycle,
    -- and it makes the token removable with the key that sets it — there is no
    -- second key for "no priority" because the ring has a stop for it.  Pure and
    -- order-only, so the two directions are one list read two ways.
  , "    const PRIORITY_RING = [null, \"C\", \"B\", \"A\"];"
  , "    const cycled = (now, step) => {"
  , "      const at = PRIORITY_RING.indexOf(now || null);"
  , "      const n = PRIORITY_RING.length;"
  , "      return PRIORITY_RING[((at === -1 ? 0 : at) + (step > 0 ? 1 : n - 1)) % n];"
  , "    };"
    -- A priority CELL as the RING spells it: the cell wears org's brackets and
    -- the ring holds the letter, so this is `priorityLetter' on the page's side
    -- of the wire — the same reading the filter and the comparator make, and a
    -- BRACKETLESS cell is taken as the letter it is.  ONE function, because the
    -- table's ring and the sheet's each had their own regexp and the sheet's
    -- refused what this accepts.
  , "    const priorityIn = (cell) => {"
  , "      const t = String(cell || \"\").trim();"
  , "      const m = /^\\[#(.)\\]$/.exec(t);"
  , "      return m ? m[1].toUpperCase() : (t ? t.toUpperCase() : null);"
  , "    };"
  , "    const priorityOf = (id) => priorityIn((rowOf(id).cells || {}).priority);"
    -- EACH ROW CYCLES FROM ITS OWN VALUE, org's per-entry semantics and the one
    -- thing a single request cannot carry: `args' is one object for the whole
    -- call, so a marked set of MIXED priorities is one command per landing value,
    -- each over the rows that land there.  A set that agrees is one request, the
    -- common press; a set that does not stays mixed and moves together, which is
    -- what a reader who marked them meant.  It is the tags popup's rule reached
    -- from another side: several flags are several commands, a command naming one
    -- value.
  , "    async function cyclePriority(b, step) {"
  , "      const ids = targets();"
  , "      if (!ids.length) { said(b, \"no row\"); return; }"
  , "      const groups = new Map();"
  , "      for (const id of ids) {"
  , "        const want = cycled(priorityOf(id), step);"
  , "        const key = want === null ? \"\" : want;"
  , "        groups.set(key, (groups.get(key) || []).concat([id]));"
  , "      }"
    -- ONE COMMAND AT A TIME, AWAITED.  Two landing values over rows that share a
    -- FILE are two requests against ONE drift lock: fired together, each is
    -- measured against a digest the other is moving, so half the press comes back
    -- refused or the later write lands over the earlier and both answer `ok'.
    -- Awaited, the refusal is deterministic and the log names it, where the race
    -- named nothing.  AND EVERY VALUE IS STILL ATTEMPTED: `fire' THROWS on a
    -- whole-request refusal, so an unguarded `await' would abandon the values
    -- behind it with no pill and no line — the flags are already spent by then,
    -- leaving the reader with some rows moved, some not, and nothing said.  The
    -- refusal is logged where it happened and the loop goes on.
  , "      for (const [key, over] of groups)"
  , "        await fire(b, \"set-priority\", over, { priority: key || null },"
  , "                   key ? `[#${key}]` : EMPTY).catch(failed(b, \"set-priority\"));"
  , "    }"
    -- Capture: the one write that names no row, so it takes none of the selection
    -- machinery above.  The line is raw org — `TODO Buy milk :errands:' captures
    -- a keyword, a title and a tag — and the server decides WHERE, out of the
    -- tree's own `#+GLANCE_CAPTURE_TARGET:'.  The row comes back over the socket
    -- once the watch has read the file it was written to, like every write here.
    --
    -- ONE POPUP, not a chain of palettes: the sequential prompts closed and
    -- reopened the overlay per step, and the swap read as a blink.  `+' raises
    -- the form whole — the tag field with the tree's vocabulary under it, then
    -- one field per `%^{PROMPT}' the tag's template asks (grown in place when
    -- the tag settles, since only the server knows them), then the line.  RET
    -- moves the focus forward and, at the line, captures; TAB is RET's quiet
    -- twin; ESC anywhere closes the form with nothing sent.  An EMPTY tag is
    -- the untagged inbox path exactly as it was.
    --
    -- The vocabulary is the server's (`/capture'), narrowed as the reader
    -- types; a name of the tree's own is committable, the charset wall being
    -- the server's.  A refusal keeps the form up — the reader fixes the line
    -- rather than retyping the form — so `shutCapture' runs on the 200 alone.
  , "    let capping = null;   // the capture form's state while it is up"
  , "    const capUp = () => !!capping;"
  , "    function shutCapture() {"
  , "      capping = null;"
  , "      el(\"kfields\").textContent = \"\";"
  , "      el(\"klist\").textContent = \"\";"
  , "      el(\"ktag\").value = \"\"; el(\"ktext\").value = \"\";"
  , "      el(\"capture\").className = \"\";"
  , "      const held = document.activeElement;"
  , "      if (held && held.blur) held.blur();"
  , "    }"
  , "    function openCapture(b) {"
  , "      sole(\"capture\");"
  , "      capping = { b, vocab: [], hot: -1, tag: null, inputs: [] };"
  , "      el(\"ktag\").value = \"\"; el(\"ktext\").value = \"\";"
  , "      el(\"kfields\").textContent = \"\"; el(\"klist\").textContent = \"\";"
  , "      showPopup(\"capture\", \"k\", \"capture\","
  , "                `RET moves on · at the line it captures · ${EMPTY} tag is the inbox · ESC leaves`);"
  , "      el(\"ktag\").focus();"
  , "      captureShape(null).then((a) => {"
  , "        if (!capping) return;"
  , "        capping.vocab = a.tags || [];"
  , "        drawTagList(el(\"ktag\").value);"
  , "      }).catch(failed(b, \"capture\"));"
  , "    }"
    -- The completion under the tag field: the vocabulary narrowed by substring
    -- over the folded spelling, at most eight shown, `C-n'/`C-p' and the
    -- vertical arrows moving a highlight RET takes.  No highlight commits the
    -- field as typed — a tag the tree has never held is reachable, exactly as
    -- the palette's `freely' rule had it.
  , "    function drawTagList(typed) {"
  , "      if (!capping) return;"
  , "      const want = foldTag(typed);"
  , "      capping.shown = capping.vocab"
  , "        .filter((t) => !want || foldTag(t).indexOf(want) !== -1).slice(0, 8);"
  , "      if (capping.hot >= capping.shown.length) capping.hot = -1;"
  , "      const box = el(\"klist\");"
  , "      box.textContent = \"\";"
  , "      capping.shown.forEach((t, i) => {"
  , "        const e = document.createElement(\"div\");"
  , "        e.className = i === capping.hot ? \"ke kh\" : \"ke\";"
  , "        e.textContent = t;"
  , "        box.appendChild(e);"
  , "      });"
  , "    }"
    -- The tag SETTLES on RET or TAB out of its field: the prompts are fetched
    -- and grown in place, and the focus moves to the first of them, else to
    -- the line.  Editing the tag afterwards clears the grown fields — they
    -- describe a template the field no longer names — and the next settle
    -- regrows them.
  , "    function settleTag() {"
  , "      const picked = capping.hot >= 0 ? capping.shown[capping.hot] : null;"
  , "      if (picked) el(\"ktag\").value = picked;"
  , "      const tag = foldTag(el(\"ktag\").value);"
  , "      capping.tag = tag; capping.hot = -1;"
  , "      el(\"kfields\").textContent = \"\"; capping.inputs = [];"
  , "      el(\"klist\").textContent = \"\";"
  , "      if (!tag) { el(\"ktext\").focus(); return; }"
  , "      captureShape(tag).then((a) => {"
  , "        if (!capping || capping.tag !== tag) return;"
  , "        for (const want of (a.prompts || [])) {"
  , "          const row = document.createElement(\"div\");"
  , "          row.className = \"krow\";"
  , "          const lab = document.createElement(\"label\");"
  , "          lab.className = \"klab\"; lab.textContent = want;"
  , "          const inp = document.createElement(\"input\");"
  , "          inp.spellcheck = false;"
  , "          row.appendChild(lab); row.appendChild(inp);"
  , "          el(\"kfields\").appendChild(row);"
  , "          capping.inputs.push({ want, inp });"
  , "        }"
  , "        (capping.inputs.length ? capping.inputs[0].inp : el(\"ktext\")).focus();"
  , "      }).catch(failed(capping.b, \"capture\"));"
  , "    }"
    -- The form's keys, a document listener like the popups': RET and TAB move
    -- the focus forward — tag, each grown field, the line — and RET at the
    -- line is the capture.  `C-n'/`C-p' and the vertical arrows walk the tag
    -- list while that field holds the focus.  ESC stays the keymap's `cancel',
    -- which reaches this surface through `SURFACES' like every other.
  , "    document.addEventListener(\"keydown\", (e) => {"
  , "      if (!capping || e.defaultPrevented) return;"
  , "      const held = document.activeElement;"
  , "      const k = keyName(e);"
  , "      if (held === el(\"ktag\")) {"
  , "        if (k === \"C-n\" || k === \"<down>\") {"
  , "          capping.hot = Math.min(capping.hot + 1, (capping.shown || []).length - 1);"
  , "          drawTagList(el(\"ktag\").value); e.preventDefault(); return;"
  , "        }"
  , "        if (k === \"C-p\" || k === \"<up>\") {"
  , "          capping.hot = Math.max(capping.hot - 1, -1);"
  , "          drawTagList(el(\"ktag\").value); e.preventDefault(); return;"
  , "        }"
  , "        if (k === \"RET\" || k === \"TAB\") { settleTag(); e.preventDefault(); }"
  , "        return;"
  , "      }"
  , "      const at = capping.inputs.findIndex((f) => f.inp === held);"
  , "      if (at !== -1 && (k === \"RET\" || k === \"TAB\")) {"
  , "        const next = capping.inputs[at + 1];"
  , "        (next ? next.inp : el(\"ktext\")).focus();"
  , "        e.preventDefault(); return;"
  , "      }"
  , "      if (held === el(\"ktext\") && k === \"RET\") {"
  , "        const fields = {};"
  , "        for (const f of capping.inputs) fields[f.want] = f.inp.value;"
  , "        captureRow(capping.b, el(\"ktext\").value, capping.tag || \"\", fields);"
  , "        e.preventDefault();"
  , "      }"
  , "    });"
    -- Typing in the tag field re-narrows the list and orphans any grown
    -- fields: they describe a template the field no longer names, and the next
    -- settle regrows the right ones.
  , "    el(\"ktag\").addEventListener(\"input\", () => {"
  , "      if (!capping) return;"
  , "      capping.hot = -1; capping.tag = null;"
  , "      el(\"kfields\").textContent = \"\"; capping.inputs = [];"
  , "      drawTagList(el(\"ktag\").value);"
  , "    });"
    -- And the write.  The line is raw org — `TODO Buy milk :errands:' captures a
    -- keyword, a title and a tag — and the server decides WHERE: the tree's own
    -- `#+GLANCE_CAPTURE_TARGET:' with no tag, a blob in the store with one.  The
    -- row comes back over the socket once the watch has read the file, like every
    -- write here, and the id the answer carries is what point lands on when it
    -- does.
  , "    function captureRow(b, text, tag, fields) {"
  , "      const typed = text.trim();"
  , "      if (!typed) { said(b, \"nothing to capture\"); return; }"
  , "      const args = { text: typed };"
  , "      if (tag) args.tag = tag;"
  , "      if (fields && Object.keys(fields).length) args.fields = fields;"
  , "      postCommand({ name: \"capture\", args }).then((a) => {"
  , "        arriving = a.id || null;"
    -- The form closes on the 200 alone: a refusal lands in the log and leaves
    -- everything typed where it was, so fixing the line is an edit rather than
    -- a retype.
  , "        shutCapture();"
  , "        said(b, tag ? `captured · :${tag}:` : `captured · ${a.file}`);"
  , "        append(\"cmd\", \"info\","
      <> " `headline ${JSON.stringify(typed)} captured into ${a.file}`);"
  , "      }).catch(failed(b, \"capture\"));"
  , "    }"
    -- Reschedule: the same shape as the state palette, over the same rows —
    -- marked set, else the row at point — with a line of text where that one has
    -- a list.  The server parses the date (ISO, `+3d', `today', or org's own
    -- bracketed form) and refuses anything else as the whole request, so an
    -- unreadable line moves no row rather than some of them.  An EMPTY line
    -- clears the entry, which is how the planning line comes off. A count of
    -- rows, pluralised: every surface naming a set of them says it the same way,
    -- so the rule sits here rather than at each of them.
  , "    const rowsWord = (n) => `${n} row${n === 1 ? \"\" : \"s\"}`;"
    -- A TAG AS A PALETTE HANDS IT BACK, folded and trimmed.  Presence is folded
    -- everywhere on this page, so the three surfaces that take a tag off a
    -- palette — the capture chain, the tags popup's add, its rename — read it
    -- through one function rather than three copies of the same two calls.
  , "    const foldTag = (t) => String(t || \"\").trim().toLowerCase();"
  , "    const tagFrom = (c) => foldTag(c.tag);"
    -- The rows a keyed write runs over, and the title that names them: the two
    -- keys that ask something before writing count them the same way.
  , "    function overTargets(b, label, k) {"
  , "      const ids = targets();"
  , "      if (!ids.length) { said(b, \"no row\"); return; }"
  , "      k(b, ids, `${label} · ${rowsWord(ids.length)}`);"
  , "    }"
    -- Its sibling over the DOCUMENT, where the set is settled: a sheet stands on
    -- ONE entry, so there is no marked set to inherit and the title names the
    -- entry rather than counting rows.  Which is the whole of what the two keys
    -- inside the sheet differ from the table's in.
  , "    const docTargets = (b, label, k) =>"
  , "      k(b, [editing.id], `${label} · ${docTitle()}`);"
    -- ASK WHICH STATE, over the rows a key worked out.  The overlay goes up on
    -- the press and the answer fills it: the same server that refuses a keyword
    -- the row's own file does not declare is the one that says which keywords
    -- those are, so the offer and the refusal cannot disagree.  A fill that
    -- lands after the reader has left finds another prompt or none, and drops.
  , "    function askState(b, ids, title) {"
  , "      const mine = ask(title,"
  , "        (c) => fire(b, \"set-state\", ids, { keyword: c.keyword },"
  , "                    c.keyword === null ? EMPTY : c.keyword),"
  , "        \"a letter sets it · / to search · ESC leaves\");"
  , "      keywordSources(ids).then((answer) => {"
  , "        if (prompting === mine) setChoices(answer.sources);"
  , "      }).catch(askFailed(mine, \"keywords\"));"
  , "    }"
    -- And which tags, which is the POPUP rather than a palette.  Raised LATE,
    -- behind the fetch, since no key inside the list it opens is also the key
    -- that opens it: an empty mount put up on the press would buy nothing and
    -- cost a raising guard.  Every named row unknown to the store leaves nothing
    -- to tag at all, which is a refusal rather than an empty popup.
  , "    function askTags(b, ids, title) {"
  , "      tagsOf(ids).then((answer) => {"
  , "        if (!(answer.rows || []).length) { said(b, \"no such row\"); return; }"
  , "        showTags(b, title, answer);"
  , "      }).catch(failed(b, \"tags\"));"
  , "    }"
  , "    function planRows(b, keyword) {"
  , "      overTargets(b, keyword.toLowerCase(), (bind, ids, title) =>"
  , "        askText(title, \"RET sets it · empty clears it · ESC leaves\", \"\", (c) => {"
  , "          const date = c.text.trim();"
  , "          fire(bind, \"set-planning\", ids, { keyword, date: date || null },"
  , "               date || \"cleared\");"
  , "        }));"
  , "    }"
    -- The value palette: a prompt of this page's own, the renderer's overlay
    -- belonging to the filter and this page not being allowed to reach into it.
    -- ESC is the keymap's own `keyboard-quit', which closes whichever overlay is
    -- up.  It opens in WHICH-KEY mode: every entry wears a letter and that letter
    -- commits on its own.  The palette IS the confirmation — a reader who pressed
    -- `t' has seen the list saying `t' sets TODO — so there is no second key, and
    -- the drift lock is what makes a mis-press cheap.  `/' falls back to the
    -- completing-read this used to be, for a cycle wide enough that some entry
    -- claimed nothing.
    --
    -- The keys are handled in a document listener of its own rather than on the
    -- field, behind the dispatch.  What holds the SHEET off it is `momentary()',
    -- which its listener asks first — `typing()' is the map's guard and reaches
    -- the map's rows alone, and the sheet's listener is not one of them.
  , "    let prompting = null;"
    -- The which-key assignment: each entry claims the first letter of its OWN
    -- spelling that no earlier entry took, over one a-z namespace in palette
    -- order.  What comes back is that letter's INDEX — the display bolds it
    -- there, which is what teaches why DELEGATED is `e' — and -1 for an
    -- entry whose every letter was taken.  Pure and order-only, so one tree's
    -- cycle always yields the same letters and the muscle memory holds.
  , "    function whichKeys(labels) {"
  , "      const taken = new Set();"
  , "      return labels.map((label) => {"
  , "        for (let i = 0; i < label.length; i += 1) {"
  , "          const c = label[i].toLowerCase();"
  , "          if (c >= \"a\" && c <= \"z\" && !taken.has(c)) { taken.add(c); return i; }"
  , "        }"
  , "        return -1;"
  , "      });"
  , "    }"
    -- A declaration rather than a `const', so a direct `eval' of this glue
    -- leaks it the way it leaks `whichKeys': the suite's harness reports the
    -- assignment through THIS function rather than re-spelling the rule.
  , "    function letterAt(label, at) {"
  , "      return at === -1 ? null : label[at].toLowerCase();"
  , "    }"
    -- The list palette, raised EMPTY on the keydown: `t' and `:' fill it from
    -- `/keywords' and `/tags', making the table the resolver's answer rather than
    -- this page's guess, and until the fill lands a reader sees the line saying
    -- so.
    --
    -- `raising' is that keydown, still in flight: this listener sits behind the
    -- dispatch, so the press that opened the palette is the next one it sees, and
    -- `t' is both the opener and a letter in what it opens.  Every palette here
    -- is raised that way, so it is set rather than passed.  The prompt itself is
    -- handed back, so a fill landing after an ESC can tell that the overlay it
    -- was asked for is gone. THE OVERLAY GOING UP, and both doors take it: the
    -- STATE the prompt is being raised in, the line its field opens on, the mode
    -- the box wears and the foot naming the keys the body cannot draw.  What
    -- differs past this is the body — one has entries and the other a line — so
    -- it is the state handed in rather than a branch here.
  , "    function raise(title, state, value, cls, foot) {"
  , "      prompting = state;"
  , "      el(\"phead\").textContent = title;"
  , "      el(\"pinput\").value = value;"
  , "      el(\"prompt\").className = \"on\";"
  , "      mode(cls, foot);"
  , "      return prompting;"
  , "    }"
  , "    function ask(title, commit, foot, over) {"
  , "      sole(over);"
  , "      return raise(title, { choices: [], shown: [], at: 0, commit,"
  , "                            narrow: false, raising: true }, \"\", \"\", foot);"
  , "    }"
    -- LIST under its letters, drawn.  The one place the which-key pool is spent,
    -- so the rule a reader learns by heart has one implementation: the state
    -- palette hands over its table flattened in draw order, the link palette its
    -- own flat list.  The letters are stamped IN PLACE, the table's cells holding
    -- these very objects and a copy leaving them holding entries as they were
    -- before one was assigned.  An entry that came in with a key of its OWN
    -- (`fixed') is out of the pool and out of the assignment: `*empty*' answers
    -- to DEL, which is no letter, so the a-z namespace is spent on KEYWORDS alone
    -- and the cycle that used to lose one to the meta keeps it.
  , "    function offer(list) {"
  , "      const pool = list.filter((c) => !c.fixed);"
  , "      whichKeys(pool.map((c) => c.label)).forEach((cut, i) => {"
  , "        pool[i].cut = cut;"
  , "        pool[i].key = letterAt(pool[i].label, cut);"
  , "      });"
  , "      prompting.choices = list;"
  , "      prompting.shown = list;"
    -- A reader who pressed `/' and typed while an answer was out is narrowing
    -- an empty list; the fill lands in the mode it finds rather than throwing
    -- the typing away.
  , "      if (prompting.narrow) narrowTo(el(\"pinput\").value);"
  , "      else drawChoices();"
  , "    }"
    -- SOURCES as the palette holds them: the labels down the table's first
    -- column, and the flat ordered list everything else reads.  The flattening is
    -- the draw order — each source row's active cell and then its inactive one,
    -- `*empty*' last — so the letters are assigned ONCE over the whole table and
    -- a letter is the reader's wherever in it the keyword sits.  It is also the
    -- list `/' narrows, so both modes offer the same entries under the same
    -- names.  The letter is folded into each entry HERE, once, and the entry is
    -- an OBJECT both halves hold: `table' keeps the cells and `choices' the flat
    -- list, the same objects, so the drawing and the dispatch read one field of
    -- one thing rather than agreeing on a parallel array's indices — `shown' is
    -- narrowed and `choices' is not.  Which is also why the letters are stamped
    -- in place: a copy would leave the cells holding the entries as they were
    -- before one was assigned.
  , "    function setChoices(sources) {"
  , "      const flat = [];"
  , "      const held = (word) => {"
  , "        const c = { label: word, keyword: word, color: badgeColor(word) };"
  , "        flat.push(c);"
  , "        return c;"
  , "      };"
    -- Every source is drawn under the name it arrives under: `default',
    -- `system', `file' and a tag all read as they are, so this page holds no
    -- table of labels to keep in step with the resolver's names.
  , "      prompting.table = (sources || []).map((s) => ({"
  , "        source: s.source,"
  , "        cells: [s.active || [], s.inactive || []].map((ws) => ws.map(held)),"
  , "      }));"
  , "      prompting.meta = { label: EMPTY, keyword: null, meta: true,"
  , "                         fixed: true, key: \"DEL\", cut: -1 };"
  , "      flat.push(prompting.meta);"
  , "      offer(flat);"
  , "    }"
    -- The same overlay with no list in it: one line of text, typed and committed
    -- with RET.  It is the minibuffer the filter palette set the pattern for, and
    -- it is this one rather than a widget of its own because everything a prompt
    -- owes — the band it paints in, the blur on the way out, ESC through the
    -- keymap's `cancel' — is already here.  `text' is what the key listener reads
    -- to know there is nothing to narrow and nothing a letter would commit; the
    -- drawing reads it too and leaves the list empty, so this prompt carries no
    -- entries at all.
  , "    function askText(title, foot, initial, commit) {"
  , "      sole();"
  , "      raise(title, { commit, text: true, raising: true }, initial, \"narrow\", foot);"
  , "      el(\"pinput\").focus();"
  , "    }"
    -- THE FIELD: the completing-read `/' falls back to over the state palette,
    -- and the whole of what `+' raises over the tags popup.  What it completes
    -- over is `wider', and a line matching nothing commits as written (`freely'),
    -- the charset wall that refuses garbage being the server's.  FOOT is the one
    -- this mode is to wear, left out where the raise already wrote it: `askFrom'
    -- hands `ask' the field's own foot, so a second copy of that string here
    -- would be one to keep in step for nothing.
  , "    function fieldMode(foot) {"
  , "      prompting.narrow = true;"
  , "      el(\"pinput\").value = \"\";"
  , "      if (prompting.wider) offer(prompting.wider);"
  , "      mode(\"narrow\", foot);"
  , "      el(\"pinput\").focus();"
  , "    }"
    -- The same overlay raised STRAIGHT into that field, with no letters behind
    -- it: `+' over the tags popup, whose list is the addable vocabulary and whose
    -- commit adds.  One widget for both doors into typing, so what a prompt looks
    -- like and how ESC leaves it are decided once.  The raising guard is CLEARED
    -- rather than left set: it declines the keydown that OPENED a palette, which
    -- is only ever a problem for a key the DISPATCH raised, and the press that
    -- reaches here came from another modal surface's own listener and has been
    -- handled already, so leaving the guard up would decline the next real key.
  , "    function askFrom(title, list, foot, commit) {"
      -- Raised OVER the popup that asked for it, which is the one raise that
      -- keeps what stood: this is that popup's own field.
  , "      const mine = ask(title, commit, foot, true);"
  , "      mine.raising = false;"
  , "      mine.wider = list;"
  , "      fieldMode();"
  , "      return mine;"
  , "    }"
    -- The chrome the mode owns — the box's class, which shows the field, and the
    -- foot naming the keys the list cannot draw for itself.  Written at the two
    -- transitions, so `drawChoices' stays a list renderer and a keystroke that
    -- narrows invalidates nothing outside the list.  An ABSENT foot leaves the
    -- one that is there: a transition changing only the mode says so by naming no
    -- foot, where passing the string it already wrote would be a second copy.
    -- TOGGLE, never assign, for `#sheet''s reason one box over: `#pbox' carries
    -- its SIZE TIER as a class too, and a wholesale write dropped it on the first
    -- raise — silently, since the markup still reads right and only a live page
    -- is a size.  `classList' spells "set one class, keep the rest".
  , "    function mode(cls, foot) {"
  , "      el(\"pbox\").classList.toggle(\"narrow\", cls === \"narrow\");"
  , "      if (foot !== undefined) el(\"pfoot\").textContent = foot;"
  , "      drawChoices();"
  , "    }"
    -- Blurred as well as hidden: a focused field nobody can see would leave
    -- `typing()' true and swallow every key after it.
  , "    function unask() {"
  , "      prompting = null;"
  , "      el(\"prompt\").className = \"\";"
  , "      el(\"pinput\").blur();"
  , "    }"
    -- The palette is the RESOLUTION, drawn as the layered table it is: one row
    -- per source in precedence order, widest first, the source named down the
    -- first column and its keywords in the Active and Inactive cells.  What a
    -- reader learns from it is why — `TODO' under `default' and `READING' under
    -- `book' is the classify chain saying which scope answered.  The hairlines
    -- are the rows' own borders and the old active/done split is the two COLUMNS;
    -- `*empty*' keeps a spanning row of its own at the foot, in the muted italic
    -- every starred meta wears, since no scope declares taking a keyword off.
    -- Three shapes, and the mode picks: the text prompt has no list at all, the
    -- fallback is the flat list under a cursor, and a table not back yet is the
    -- line saying so.
  , "    function drawChoices() {"
  , "      const list = el(\"plist\");"
  , "      list.textContent = \"\";"
  , "      if (prompting.text) return;"
  , "      if (prompting.narrow) {"
  , "        prompting.shown.forEach((c, i) => entry(list, \"pe\""
  , "          + (c.meta ? \" pm\" : \"\") + (i === prompting.at ? \" pat\" : \"\"), c));"
  , "        return;"
  , "      }"
    -- A list that is empty in LETTER mode is a resolution that has not landed:
    -- the overlay goes up on the keydown and the answer fills it, and this is
    -- the line a reader sees until it does.
  , "      if (!prompting.choices.length) {"
  , "        part(list, \"div\", \"pnone\", \"resolving…\");"
  , "        return;"
  , "      }"
    -- Past those two the list IS the resolution table: letter mode is the state
    -- palette's alone now, and `setChoices' is the only thing that fills one.
  , "      const head = part(list, \"div\", \"pr ph\");"
  , "      part(head, \"div\", \"ps\", \"source\");"
  , "      part(head, \"div\", \"pc\", \"active\");"
  , "      part(head, \"div\", \"pc\", \"inactive\");"
  , "      prompting.table.forEach((src) => {"
  , "        const row = part(list, \"div\", \"pr\");"
  , "        part(row, \"div\", \"ps\", src.source);"
  , "        src.cells.forEach((cell) => {"
  , "          const box = part(row, \"div\", \"pc\");"
  , "          cell.forEach((c) => entry(box, \"pe\", c));"
  , "        });"
  , "      });"
  , "      entry(part(list, \"div\", \"pr pm\"), \"pe\", prompting.meta);"
  , "    }"
    -- One entry: the key token, then the keyword in its badge colour with the
    -- claimed letter BOLD where it sits.  The token column goes in the fallback
    -- mode, and the bolding with it: no letter commits there, and drawing one
    -- would be a lie about what typing it does.
  , "    function entry(into, cls, c) {"
  , "      const row = part(into, \"div\", cls);"
    -- The letter is marked IN the word, so there is no token beside it — and
    -- one exception: a FIXED key names no position in a word (`*empty*' answers
    -- to DEL), so that entry alone keeps a token to be told by.  In the
    -- fallback mode nothing commits by key at all, so nothing is marked.
  , "      const marked = !prompting.narrow && c.cut >= 0;"
  , "      if (!prompting.narrow && c.fixed) part(row, \"span\", \"pk\", c.key);"
  , "      const word = part(row, \"span\", \"pw\");"
  , "      if (c.color) word.style.color = c.color;"
  , "      if (!marked) word.textContent = c.label;"
  , "      else {"
  , "        part(word, \"span\", \"\", c.label.slice(0, c.cut));"
    -- The rule under the letter takes the keyword's own hue, which only the
    -- entry knows; the weight and the thickness are the stylesheet's.
  , "        const hot = part(word, \"b\", \"\", c.label[c.cut]);"
  , "        if (c.color) hot.style.textDecorationColor = c.color;"
  , "        part(word, \"span\", \"\", c.label.slice(c.cut + 1));"
  , "      }"
    -- The muted aside beside an entry: the tag palette's partial count (`2/3'),
    -- which is what says a tag only some of the set carries.  A keyword has
    -- none, and neither has a levelled tag.
  , "      if (c.hint) part(row, \"span\", \"pt\", c.hint);"
  , "    }"
  , "    function narrowTo(text) {"
  , "      const want = text.trim().toLowerCase();"
    -- Over the LABEL alone.  The aside is drawn rather than searched: the add
    -- field writes a coverage count into it and a digit must not narrow the
    -- list to the entries that happen to be 2-of-3.
  , "      prompting.shown = prompting.choices.filter((c) =>"
  , "        c.label.toLowerCase().includes(want));"
  , "      prompting.at = 0;"
  , "      drawChoices();"
  , "    }"
  , "    function walkChoices(step) {"
  , "      const n = prompting.shown.length;"
  , "      if (n) prompting.at = Math.max(0, Math.min(n - 1, prompting.at + step));"
  , "      drawChoices();"
  , "    }"
    -- ONE COMMIT for every mode: a letter, and the field's own RET.  The overlay
    -- comes down FIRST, so what the commit sees is a page with no prompt on it —
    -- a palette that stayed open over its own write is the shape the tags
    -- palette took, and that list is a mount now.
  , "    function takeChoice(chosen) {"
  , "      if (!chosen) return;"
  , "      const act = prompting.commit;"
  , "      unask();"
  , "      act(chosen);"
  , "    }"
    -- The typed line as an entry, for a field whose typing REACHES PAST its
    -- list — which is what `wider' says: a tag the tree has never held has to be
    -- committable, since that is the only way a first one is ever written.
  , "    const freely = () => {"
  , "      if (!prompting.wider) return null;"
  , "      const typed = el(\"pinput\").value.trim();"
  , "      return typed ? { tag: typed } : null;"
  , "    };"
    -- The two fields that hold a LINE rather than a filter narrow nothing: the
    -- text prompt has no list, and `+'\''s field is a name being written rather
    -- than one being looked for.
  , "    el(\"pinput\").addEventListener(\"input\", (e) =>"
  , "      prompting && !prompting.text && narrowTo(e.target.value));"
  , "    el(\"prompt\").addEventListener(\"click\", (e) =>"
  , "      { if (e.target === el(\"prompt\")) unask(); });"
    -- What C-c C-t offers: the states the SERVER says those rows may be set to,
    -- with the scope that declares each — org's own cycle under `default',
    -- `system.org', the row's tags' configs, its file's own `#+TODO:' — plus the
    -- entry that takes a keyword off.  Resolved per request, the answer being per
    -- ROW: the state column's badges are the union of every file loaded, a
    -- superset saying nothing about where a keyword came from, and the column's
    -- `values' are the filter's group meta-values (`*active*'), absent from both
    -- because no file declares one, so the server refuses every one of them and
    -- offering a value that cannot be set is worse than not offering it.
    -- `*empty*' wears the stars every reserved meta wears (docs/invariants.md)
    -- and is the filter's own word for a cell holding nothing — the entry takes
    -- the state column's cell to exactly what `state:*empty*' then finds.  The
    -- starred form is the page's mark for a value with semantics rather than a
    -- word a file could hold, and the server refuses a starred string as a
    -- keyword from the other side.  What it commits is a null keyword, and the
    -- key it answers to is DEL — a key that already MEANS take-it-off wherever
    -- this page binds one, and no letter, so the a-z pool is spent on KEYWORDS
    -- alone and a cycle wide enough to run the pool dry keeps the letter the meta
    -- used to take.  In the typing mode DEL is the field's own and `*empty*' is
    -- reached the way every other entry is, by narrowing to it.
  , "    const EMPTY = \"*empty*\";"
    -- The colour is the badge's own, so a keyword reads in the palette as it
    -- reads in the table.  Looked up rather than carried: the resolution names
    -- keywords, and the hues are the producer's and ride on the state column
    -- where every other reader of them finds them.
  , "    const badgeColor = (keyword) =>"
  , "      (((cols.find((c) => c.key === \"state\") || {}).badges || [])"
  , "        .find((b) => b.value === keyword) || {}).color || \"\";"
    -- ONE parameter per id rather than the comma list a caller types by hand:
    -- the fallback row id is a path and a comma in one would split it, and
    -- percent-encoding cannot help — the server splits after decoding.  Both
    -- id-taking routes ask it the same way, so the rule is spelled once.
  , "    const askIds = (route, ids) =>"
  , "      getJSON(route + \"?\""
  , "        + ids.map((i) => \"ids=\" + encodeURIComponent(i)).join(\"&\"));"
  , "    const keywordSources = (ids) => askIds(\"/keywords\", ids);"
    -- Where a row points, out of the server's reading of its subtree.  This
    -- page holds no org parser, so the bracket grammar stays where the display
    -- rule already lives — one link is `[[TARGET][DESC]]' shown as DESC, and a
    -- bare URL is its own description.
  , "    const linksOf = (id) => getJSON(`/links?id=${encodeURIComponent(id)}`);"
    -- What a capture under TAG will ask for, and the vocabulary and codes that
    -- come with every answer.  A null tag is the untagged path's own shape.
  , "    const captureShape = (tag) =>"
  , "      getJSON(tag === null ? \"/capture\" : `/capture?tag=${encodeURIComponent(tag)}`);"
    -- What the rows a tag command names are tagged with, and what else the tree
    -- holds.  Per row rather than as a union, because WHICH rows lack a tag is
    -- what decides where an add is sent; the union and its partial counts are
    -- worked out here, off that.
  , "    const tagsOf = (ids) => askIds(\"/tags\", ids);"
    -- What a browser tab can be pointed at, which is http(s) and nothing else.
    -- Org writes plenty of other link types and `/links' reports them all —
    -- `mailto:', `file:', org's `id:', org-glance's own protocols, a bare
    -- `[[Title]]' naming a headline — each naming something a tab is not.
    -- Following one needs a handler this page does not have yet, so it says so
    -- instead of opening a tab on a string a browser will make nothing of.
    --
    -- The TYPE decides, the server's own word for the target
    -- (`Glance.Query.linkType'): the scheme, folded.  A regex over the target
    -- here would be this page deriving a second time what the answer already
    -- carries, leaving the popup's badge column and this test two readings of one
    -- string.  The LIST is the server's too (`Glance.Query.followableTypes'),
    -- spliced the way `PLANNING' is, and is the same list the badge palette gives
    -- the warm hues to, so what reads as followable and what opens cannot come
    -- apart.
  , "    const FOLLOWABLE = " <> jsonValue followableTypes <> ";"
    -- THE EXPANSION SUBSET, spliced in like `FOLLOWABLE' rather than fetched:
    -- it is a property of the BUILD rather than of the tree, so the binary that
    -- expands a template is the binary that says what it expands, and a stale
    -- list has no way to exist.  `GET \/capture' still carries it — that is the
    -- wire contract, and a client this page is not still reads it there.
  , "    const CODES = " <> jsonValue codeList <> ";"
  , "    const followable = (l) => FOLLOWABLE.indexOf(l.type) !== -1;"
    -- A target in a log line, kept to a width the strip can show: an org link
    -- target runs to a hash and a path, and the line has other words in it.
  , "    const shortly = (t) => {"
  , "      const s = String(t || \"\");"
  , "      return s.length > 80 ? s.slice(0, 79) + \"…\" : s;"
  , "    };"
    -- One tab, and the log keeps what was followed: a link opened is the one
    -- thing a key here does that leaves no trace on the page it was pressed on.
    -- `noopener' because the opened page must not reach back into this one.
    --
    -- The COMMIT is where a link type is judged, which is why this is one
    -- function rather than a filter over the rows: the popup lists everything the
    -- row points at, that being what teaches a reader what is in the entry, and a
    -- single link takes this same door without a popup at all — so `o' on a row
    -- holding one `mailto:' warns and opens nothing.
    -- THE GESTURE `o' IS, at either grain: none is a refusal, one opens, and
    -- several raise the popup.  The table follows the ROW's links and the
    -- document the ELEMENT's, so what differs is the set handed in — the answer
    -- travels with it, the popup editing under the digest that answer carried.
  , "    function followLinks(b, id, a, links) {"
  , "      if (!links.length) { said(b, \"no links\"); return; }"
  , "      if (links.length === 1) { openLink(b, links[0]); return; }"
  , "      showLinks(b, id, a);"
  , "    }"
  , "    function openLink(b, link) {"
  , "      if (!followable(link)) {"
  , "        said(b, \"link type not implemented\");"
  , "        append(\"cmd\", \"warn\","
      <> " `link type not implemented: ${shortly(link.target)}`);"
  , "        return;"
  , "      }"
  , "      window.open(link.target, \"_blank\", \"noopener\");"
    -- The description, and never a fallback to the target: `/links' has already
    -- applied the display rule, so a bare URL arrives described by itself and an
    -- empty `desc' is a shape the route cannot produce.
  , "      said(b, link.desc);"
  , "      append(\"cmd\", \"info\", `link ${JSON.stringify(link.target)} opened`);"
  , "    }"
    -- THE LINK POPUP, a table-view MOUNT — the page's third, after the table and
    -- the sheet's property panel.  What a row points at is a LIST OF RECORDS
    -- rather than a set of commands: each link has a kind, a name and a
    -- destination, and reading them is how a reader decides which one they meant.
    -- A which-key letter is the right shape for a fixed vocabulary committed from
    -- memory (a keyword, a tag) and the wrong one for a list you have to READ,
    -- where the letters are noise over the columns that carry the answer.  So
    -- this one browses: move, look, RET.
    --
    -- READ-ONLY, and stated rather than inherited: no marks, no flags, no
    -- pageSize and no hint line.  Nothing here writes, so a gutter, a wash and a
    -- per-row hint would each be chrome about a gesture the popup does not have.
    -- `palette: true' for the property panel's reason — a handful of links is
    -- not something a reader narrows, and the filter overlay it leaves behind is
    -- never raised.  Mounted once and kept, like the panel: a mount per press
    -- would leave a theme listener behind every time the reader followed a row.
  , "    const LCOLS = " <> jsonValue linkColumns <> ";"
    -- `opening' is the BINDING that raised this — `o' or `!' — and it is also
    -- WHETHER the popup is up: a raise sets it, `shutLinks' clears it, and one
    -- value answers both questions rather than two that have to move in step.
    -- It is what lets the pill name the command that ran when the commit finally
    -- lands, the way it does for a link the row held only one of.
  , "    let lmount = null, lrows = [], opening = null, lfor = null, lpin = \"\";"
  , "    const linking = () => !!opening;"
  , "    function linksMounted() {"
  , "      if (lmount) return lmount;"
  , "      lmount = mountOnce(\"ltable\", LCOLS,"
  , "        { palette: true, marks: false, flags: false, actionHints: false },"
  , "        \"lpane\");"
  , "      return lmount;"
  , "    }"
    -- The answer as rows, under B — the binding that asked — for the row ID.
    -- The link's own id is its PLACE in the answer, the only identity a link has
    -- here: two entries may share a description, and the targets are deduplicated
    -- upstream.  The model keeps it beside the link, so finding the link the
    -- cursor is on is a lookup by id like the property panel's rather than this
    -- page taking an id apart.  TWO THINGS BESIDE THE ROWS make `RET' a WRITE:
    -- the row the links belong to, since a command names rows and this popup is
    -- raised over one, and the answer's DIGEST, the file as the store read it
    -- when it measured these spans.  An edit pins that digest, so a file that has
    -- moved since refuses rather than splicing a range that has.
  , "    function showLinks(b, id, answer) {"
  , "      sole();"
  , "      const links = answer.links || [];"
  , "      lrows = links.map((l, i) => ({ id: `L${i}`, link: l }));"
  , "      lfor = id;"
  , "      lpin = answer.digest || \"\";"
  , "      const m = linksMounted();"
  , "      m.setRows(lrows.map((r) => ({ id: r.id,"
  , "        cells: { type: r.link.type, title: r.link.desc, url: r.link.target } })));"
  , "      showPopup(\"links\", \"l\", `open · ${links.length} links`,"
  , "                \"RET edits · o opens it · ESC leaves\");"
  , "      opening = b;"
  , "      if (lrows.length) m.select(lrows[0].id);"
  , "    }"
    -- An open edit is a rung UNDER the popup, and closing the popup takes it
    -- with it; nothing else is focused — the popup holds the keys with no field
    -- in it, the way the property panel's nav does — so the rest of closing is
    -- the class coming off.  BOTH popups shut alike, so that pair is one
    -- function and each caller adds only the state whose emptiness IS its
    -- `up()': `opening' for the links, `tagging' for the tags.
  , "    function shutPopup(id, shape) { shutEdit(shape); el(id).className = \"\"; }"
    -- And RAISED alike: the head takes the words this raise gives it, the foot
    -- takes them where the surface has one to say, and the class goes on.  P
    -- names the parts (`lhead'/`lfoot', `thead'/`tfoot'), so a third popup
    -- joins by naming its own prefix.  `sole()' stays each caller's FIRST line
    -- and not this function's: it closes every momentary surface including the
    -- one being raised, so run from here it would wipe the state the caller has
    -- just written.
  , "    function showPopup(id, p, head, foot) {"
  , "      el(p + \"head\").textContent = head;"
  , "      if (foot !== undefined) el(p + \"foot\").textContent = foot;"
  , "      el(id).className = \"on\";"
  , "    }"
  , "    function shutLinks() {"
  , "      shutPopup(\"links\", LROW);"
  , "      opening = null; lfor = null; lpin = \"\";"
  , "    }"
    -- The row the cursor is on, out of the renderer's own selection — this page
    -- keeps no copy of where a popup is standing, the same rule the table and the
    -- panel follow.  The ROW rather than the link, the overlay opening over the
    -- row and the link being what it holds.
  , "    function pointedRow() {"
  , "      const at = selectedId(lmount);"
  , "      return lrows.find((r) => r.id === at) || null;"
  , "    }"
  , "    const pointedLink = () => (pointedRow() || {}).link || null;"
    -- THE LINK OVERLAY: `openEdit' over TWO cells.  The description and the
    -- target become fields over themselves, `TAB' hops, `RET' commits and `ESC'
    -- restores — the property panel's edit model exactly, and the third surface
    -- to declare a shape for it.  The type column is DERIVED (the server's word
    -- for the target, which the write itself may move), so it never opens and the
    -- box covers the two cells that can.  The target takes the focus, for the
    -- panel's reason: editing a link already there is nearly always editing where
    -- it points.
  , "    const LROW = {"
  , "      box: \"ledit\", pane: \"lpane\", fields: [\"ltitle\", \"lurl\"],"
  , "      cells: [\"title\", \"url\"], cols: LCOLS,"
  , "      mount: () => lmount,"
  , "      fill: (r) => {"
  , "        el(\"ltitle\").value = r.link.desc;"
  , "        el(\"lurl\").value = r.link.target;"
  , "      },"
  , "      focus: () => { el(\"lurl\").focus(); el(\"lurl\").select(); },"
  , "    };"
  , "    const lediting = () => !!edit && edit.o === LROW;"
    -- RET OVER NOTHING SAYS SO, and both popups say it the same way: the row the
    -- cursor is on or the command's own name for having none.  One guard, so a
    -- surface that grew an overlay cannot forget the empty case.
  , "    const openOver = (shape, at, none) =>"
  , "      (at ? openEdit(shape, at) : echo(`RET → ${none}`));"
  , "    const openLinkEdit = () =>"
  , "      openOver(LROW, pointedRow(), \"org-insert-link (no link)\");"
  , "    const cancelLinkEdit = () => cancelEdit(\"link\", LROW);"
    -- THE COMMIT, ONE row and ONE SPAN: the range `/links' handed out, spliced
    -- under the digest that answer carried.  The link is the one the overlay
    -- OPENED over — the snapshot every surface on this mechanism gets — so a
    -- click that moved the cursor under an open field cannot redirect the write.
    --
    -- The popup CLOSES on the press, both outcomes alike, which is `o'\''s own
    -- rule: picking one link is what it was raised to do.  It also has to — the
    -- spans it is holding describe the file as it was and the write has just
    -- moved it, and the store does not know yet either (`/command' never writes
    -- it, the watch does, a debounce later), so a popup left standing would be
    -- offering ranges into a text that is gone and a re-read here would answer
    -- with what the file said BEFORE the write.  `o' again is one keystroke and
    -- comes back with fresh spans.
    --
    -- ABSENT IS NOT NULL.  The description field opens on what the link SHOWS,
    -- which for a link carrying none of its own is its target, so sending that
    -- back unchanged would spell the target into a description.  Only a field
    -- the reader MOVED says anything: left alone it is absent and the link keeps
    -- what it has, emptied it is the null that takes the description off.
  , "    function commitLink(row) {"
  , "      const link = row.link;"
  , "      const target = String(el(\"lurl\").value).trim();"
  , "      const typed = String(el(\"ltitle\").value).trim();"
  , "      const b = opening, id = lfor, pin = lpin;"
  , "      shutLinks();"
  , "      if (!target) { said(b, \"a link points somewhere\"); return; }"
  , "      const args = { span: link.span, target };"
  , "      if (typed !== link.desc) args.desc = typed || null;"
  , "      if (target === link.target && args.desc === undefined)"
  , "        { said(b, \"unchanged\"); return; }"
  , "      fire(b, \"edit-link\", [id], args,"
      <> " `link edited: ${shortly(link.target)} → ${shortly(target)}`, null,"
  , "           { [id]: pin });"
  , "    }"
    -- THE TAGS POPUP, the page's FOURTH table-view mount and the first one that
    -- WRITES.  What a set of rows is tagged with is a list of RECORDS — a name, a
    -- coverage over the set, a weight in the tree — and a reader deciding whether
    -- to drop one is READING those three: the link popup's shape rather than the
    -- value palette's, so the which-key letters that used to carry this list went
    -- with the list, a letter being right for a fixed vocabulary committed from
    -- memory, which is what a KEYWORD is and a tag over a set of rows is not.
    --
    -- MUTABLE, which is the whole of what makes it a fourth mount rather than a
    -- second link popup.  `d'/`D' remove, `+' adds, `RET' renames — three
    -- gestures this page already spells elsewhere, borrowed rather than
    -- invented: dired's flag-then-confirm from the table and the property panel,
    -- the value palette's completing field, and the panel's edit overlay.
    --
    -- Marks are OFF: the set a tag command runs over is the TABLE's and was
    -- decided before this went up, so a second selection here would be a second
    -- answer to a settled question.  Flags are ON, the removal being the
    -- two-press gesture and the flag its confirmation.
  , "    const TCOLS = " <> jsonValue tagColumns <> ";"
    -- The popup's whole state: the target rows as the server described them (each
    -- with its own folded tag list), the tree's vocabulary and its store-wide row
    -- counts, and the binding that raised this — which is also WHETHER it is up,
    -- the way `opening' is for the link popup.  What the mount is SHOWING is not
    -- among them: a tag is its own row id, so `tagUnion()' answers every question
    -- a copy of the row list could, and a copy is one more thing each write has
    -- to remember to refresh.
  , "    let tmount = null, ttargets = [], tvocab = [], tcount = {};"
  , "    let tagging = null;"
  , "    const managing = () => !!tagging;"
    -- Mounted once and kept, for the panel's reason.
  , "    function tagsMounted() {"
  , "      if (tmount) return tmount;"
  , "      tmount = mountOnce(\"ttable\", TCOLS,"
  , "        { palette: true, marks: false, flags: true, actionHints: false,"
  , "          flagHelp: \"d/D remove · u unflag\" },"
  , "        \"tpane\");"
  , "      return tmount;"
  , "    }"
    -- THE UNION over the target rows, FIRST-SEEN: each row's tags in the order
    -- its file spells them, the rows in the order the server named them.
    -- Alphabetical would be no harder and strictly worse — the cursor sits on a
    -- row, and an insert in the middle moves the row out from under it, where an
    -- append cannot.
  , "    function tagUnion() {"
  , "      const seen = [];"
  , "      for (const r of ttargets) for (const t of r.tags)"
  , "        if (seen.indexOf(t) === -1) seen.push(t);"
  , "      return seen;"
  , "    }"
    -- Which of the targets carry TAG, which is what every write here is aimed
    -- at: a removal goes to the rows carrying it, an add to the rows lacking it,
    -- so what an answer counts is rows that MOVED.
  , "    const carriers = (tag) => ttargets.filter((r) => r.tags.indexOf(tag) !== -1);"
    -- COVERAGE: `all' where every target carries it, `k/n' where some do.  It is
    -- what the letter palette wrote into a muted aside, promoted to a column of
    -- its own now that there is a table to put it in.
  , "    const coverage = (tag) => {"
  , "      const on = carriers(tag).length;"
  , "      return on === ttargets.length ? \"all\" : `${on}/${ttargets.length}`;"
  , "    };"
    -- One tag as the mount holds it, and a tag IS its own id: one entry per tag
    -- per popup, so a flag, the cursor and a rename all name the same thing
    -- after any number of writes.
  , "    const tagRow = (tag) =>"
  , "      ({ id: tag, cells: { title: tag, on: coverage(tag),"
  , "                           rows: tcount[tag] === undefined ? \"\" : tcount[tag] } });"
    -- Every change to the model ends here.  AT is the tag to land the cursor on
    -- and is left out where it should stay where it is.  The union is read once
    -- and the three answers below are folds over it.
  , "    function repaintTags(at) {"
  , "      const m = tagsMounted();"
  , "      const tags = tagUnion();"
  , "      m.setRows(tags.map(tagRow));"
      -- The foot names what a reader can do, and a popup with nothing in it
      -- names the one key that still can: an untagged set is honest rather than
      -- empty, and `+' is the way in.
  , "      el(\"tfoot\").textContent = tags.length"
  , "        ? \"RET renames · d flags · D removes · + adds · ESC leaves\""
  , "        : \"nothing tagged here · + adds one · ESC leaves\";"
  , "      if (at && tags.indexOf(at) !== -1) m.select(at);"
  , "    }"
    -- Raised on the ANSWER, like the link popup and unlike the state palette: no
    -- key inside this list is also the key that opens it, so nothing is gained by
    -- putting an empty mount up first and no raising guard is owed.  TITLE is the
    -- count of the ids the command was aimed at, which is what the reader asked
    -- for; the coverage denominator is the rows the store actually answered for.
  , "    function showTags(b, title, answer) {"
  , "      sole();"
  , "      ttargets = (answer.rows || []).map((r) =>"
  , "        ({ id: r.id, tags: (r.tags || []).slice() }));"
  , "      tvocab = answer.vocabulary || [];"
  , "      tcount = answer.counts || {};"
  , "      tagging = b;"
      -- Written ONCE, here: the title is the count of the ids the command was
      -- aimed at and cannot move while the popup is up, so a repaint has no
      -- business restating it.
  , "      showPopup(\"tags\", \"t\", title);"
  , "      repaintTags(tagUnion()[0]);"
  , "    }"
    -- Nothing to blur once the rename is shut: the popup holds the keys with no
    -- field in it, the way the link popup and the property panel's nav do.
  , "    function shutTags() {"
  , "      shutPopup(\"tags\", TROW);"
  , "      tagging = null; ttargets = [];"
  , "    }"
    -- The tag the cursor is on, out of the renderer's own selection, by the rule
    -- the table, the panel and the link popup follow.  Checked against the UNION,
    -- what the mount was drawn from: a selection the popup no longer holds a row
    -- for — a shut popup's included, `ttargets' emptying the union — is no tag.
  , "    const tagAt = () => {"
  , "      const at = selectedId(tmount);"
  , "      return tagUnion().indexOf(at) !== -1 ? at : null;"
  , "    };"
    -- THE ADDABLE VOCABULARY, what `+' completes over: every tag this tree holds
    -- LESS the ones already on every target.  Adding one of those writes nothing,
    -- so offering it is offering a no-op — where one only SOME of them carry
    -- stays offered, wearing its `2/3', since adding it levels the set up.  The
    -- set's own partial tags lead and the rest of the TREE follows: the rows a
    -- page shows are a fraction of the store, so the vocabulary is the server's
    -- answer rather than a scan of what is in hand.
  , "    function addable() {"
  , "      const union = tagUnion();"
      -- `all' is exactly the coverage of a tag every target carries, so the
      -- filter reads that cell rather than counting the carriers a second time.
  , "      return union.map((t) => ({ label: t, tag: t, hint: coverage(t) }))"
  , "        .filter((c) => c.hint !== \"all\")"
  , "        .concat(tvocab.filter((t) => union.indexOf(t) === -1)"
  , "          .map((t) => ({ label: t, tag: t, hint: \"\" })));"
  , "    }"
    -- The ids one answer landed on, which is where every fold below starts.
    -- What the list shows next comes out of the command's OWN per-id answer,
    -- never a re-read: `/command' does not write the store — the watch does, a
    -- debounce later — so asking `/tags' again here would answer with what the
    -- files said BEFORE the write.
  , "    const landedIds = (results) =>"
  , "      new Set((results || []).filter((x) => x.ok).map((x) => x.id));"
    -- And the store-wide count stepped by what landed.  Arithmetic on the answer
    -- for the same reason the tag sets are, and corrected by the next
    -- resolution: the number is the tree's and only the tree can be right about
    -- it, but a column standing still while the rows under it moved would read
    -- as a stale answer rather than as a different question.
  , "    const stepCount = (tag, by) =>"
  , "      (tcount[tag] = Math.max(0, (tcount[tag] || 0) + by));"
    -- WHAT EVERY TAG WRITE DOES WITH ITS ANSWER, in one place: a popup the reader
    -- has already left is dropped, the ids that landed are folded into the model
    -- by APPLY — which is the whole of what the three commands differ in — and
    -- the list is redrawn with the cursor on AT.  Three copies of that frame is
    -- three chances to forget the guard, and forgetting it writes rows into a
    -- popup that is not there.
  , "    const landing = (at, apply) => (results) => {"
  , "      if (!managing()) return;"
  , "      apply(landedIds(results));"
  , "      repaintTags(at);"
  , "    };"
    -- `+' — the add flow, one field over the addable vocabulary and the only door
    -- into it.  It only ever ADDS, so the write goes to the rows LACKING the tag;
    -- a tag every target already carries costs a line in the pill and no round
    -- trip.  FOLDED at commit, because presence is: `/tags' reports what
    -- `tagsOfCell' reads, and a popup that wrote `Work' would go on showing
    -- `work' and offering to add it again.
  , "    const addFlow = () => askFrom(`add a tag · ${rowsWord(ttargets.length)}`,"
  , "      addable(), \"RET adds it · C-n/C-p walks · ESC leaves\", addTag);"
  , "    function addTag(c) {"
  , "      const tag = tagFrom(c);"
  , "      if (!managing() || !tag) return;"
  , "      const over = ttargets.filter((r) => r.tags.indexOf(tag) === -1);"
  , "      if (!over.length) { said(tagging, `:${tag}: is on every row already`); return; }"
  , "      fire(tagging, \"add-tag\", over.map((r) => r.id), { tag },"
  , "           `tagged :${tag}:`).then(landing(tag, (landed) => {"
  , "        for (const r of ttargets)"
  , "          if (landed.has(r.id) && r.tags.indexOf(tag) === -1) r.tags.push(tag);"
      -- A tag written for the first time joins the tree's vocabulary here, so
      -- the field offers it before the watch has told this page anything.
  , "        if (landed.size && tvocab.indexOf(tag) === -1) tvocab.push(tag);"
  , "        stepCount(tag, landed.size);"
  , "      }));"
  , "    }"
    -- `D', and the second `d' that reaches the same handler: every FLAGGED tag
    -- comes off every target carrying it.  ONE COMMAND PER TAG, a command naming
    -- one — each its own per-file batch of atomic writes.  The flags were spent
    -- before this ran (`flagKey'), and the count the other two surfaces word is
    -- nothing this can use, so it takes the ids and drops it.  AWAITED, for
    -- `cyclePriority''s reason: two tags coming off rows that share a FILE are
    -- two requests against one drift lock, and fired together each is measured
    -- against a digest the other is moving.  Guarded for its other reason too — a
    -- refusal on one tag must not abandon the tags behind it.
  , "    async function removeTags(list) {"
  , "      for (const tag of list)"
  , "        await Promise.resolve(untag(tag)).catch(failed(tagging, \"remove-tag\"));"
  , "    }"
  , "    function untag(tag) {"
  , "      const over = carriers(tag);"
  , "      if (!over.length) return;"
  , "      return fire(tagging, \"remove-tag\", over.map((r) => r.id), { tag },"
  , "           `untagged :${tag}:`).then(landing(null, (landed) => {"
  , "        for (const r of ttargets)"
  , "          if (landed.has(r.id)) r.tags = r.tags.filter((t) => t !== tag);"
  , "        stepCount(tag, -landed.size);"
  , "      }));"
  , "    }"
    -- `RET' — the rename, and ONE command.  `rename-tag' replaces the entry where
    -- the author put it, under one drift lock per file; a remove and an add fired
    -- in turn would be two writes, two locks, and a tag that moved to the end of
    -- every run it was in.  It goes to the targets carrying FROM, the set the
    -- write is for.  FROM is the tag the overlay OPENED over, read out of the
    -- snapshot before the overlay comes down: a click that moved the cursor under
    -- an open field must not rename the tag the reader landed on.
  , "    function renameTag(from, typed) {"
  , "      const to = foldTag(typed);"
  , "      shutEdit(TROW);"
  , "      if (!from || !to || to === from) { said(tagging, \"unchanged\"); return; }"
  , "      const over = carriers(from);"
  , "      fire(tagging, \"rename-tag\", over.map((r) => r.id), { from, to },"
  , "           `renamed :${from}:→:${to}:`).then(landing(to, (landed) => {"
      -- A row carrying BOTH ends loses `from' and gains nothing — the server
      -- cuts rather than renames there — so counting it would leave the tree
      -- count one high for as long as the popup stands.
  , "        const gained = ttargets.filter((r) =>"
  , "          landed.has(r.id) && r.tags.indexOf(to) === -1).length;"
  , "        for (const r of ttargets)"
  , "          if (landed.has(r.id)) r.tags = renamedTags(r.tags, from, to);"
  , "        if (landed.size && tvocab.indexOf(to) === -1) tvocab.push(to);"
  , "        stepCount(to, gained);"
  , "        stepCount(from, -landed.size);"
  , "      }));"
  , "    }"
    -- One row's tags after the rename, IN PLACE and deduplicated — the server's
    -- own rule ('Glance.Query.renameTagEdits'): the entry stays where it was, so
    -- the union's first-seen order does not shuffle under the cursor, and a row
    -- that carried both ends comes out carrying one.
  , "    const renamedTags = (tags, from, to) =>"
  , "      [...new Set(tags.map((t) => (t === from ? to : t)))];"
    -- THE RENAME OVERLAY: `openEdit' over one CELL.  The tag cell becomes a field
    -- over itself, `RET' commits and `ESC' restores.  The other two columns are
    -- DERIVED — a coverage and a count — so there is nothing in them to edit and
    -- they never open, exactly as the link popup's type cell does not; `cells'
    -- says which of them the box covers.  The tag the overlay opened on is
    -- `edit.row', the snapshot every surface using this mechanism gets.
  , "    const TROW = {"
  , "      box: \"tedit\", pane: \"tpane\", fields: [\"tname\"],"
  , "      cells: [\"title\"], cols: TCOLS,"
  , "      mount: () => tmount,"
  , "      fill: (tag) => (el(\"tname\").value = tag),"
  , "      focus: () => { el(\"tname\").focus(); el(\"tname\").select(); },"
  , "    };"
  , "    const renaming = () => !!edit && edit.o === TROW;"
  , "    const openRename = () =>"
  , "      openOver(TROW, tagAt(), \"org-rename-tag (no tag)\");"
  , "    const cancelRename = () => cancelEdit(\"tag\", TROW);"
    -- The popup's phrases for `flagKey', the gesture itself being the property
    -- panel's.  `tagAt' already answers with an id or null, which is what the
    -- shape asks for; `removeTags' names one tag per command and has no use for
    -- the count the panel spells, so it takes the ids and drops it.
  , "    const TFLAGS = {"
  , "      mount: () => tmount, at: tagAt, take: removeTags, note: unlogged,"
  , "      walk: () => stepIn(tmount, 1),"
  , "      missing: \"this table-view.js has no delete flags\","
  , "      none: \"org-toggle-tag (no tag)\","
  , "      unflag: \"tag-unflag (flag cleared)\","
  , "      flag: \"tag-flag (d again removes)\","
  , "    };"
    -- Settings, in PANELS: the general preferences, the theme, then one box per
    -- keyword layer — a layer being one config file and its `#+TODO:' lines
    -- VERBATIM.  The line is the contract org itself reads, so it is what is
    -- edited: a chip UI here would be this page guessing at a grammar it has no
    -- parser for, and the guess would be what gets written.
    --
    -- The sheet is the materialize sheet's pattern, down to the words: no
    -- buttons, ESC or the backdrop syncs and closes, `C-x C-s' syncs mid-edit,
    -- and the header carries one of the same four states.  The REQUEST is its
    -- own — `/config' is a pair of routes of its own — and the rows arrive the
    -- way every other write's do, the file watch seeing the config change and
    -- reseeding the tree.  The theme panel asks nobody, being a `localStorage'
    -- preference that applies as it is picked.
    --
    -- ONE STRUCTURE for the panels: a header and the elements under it, in
    -- order, so a panel joins by adding an entry and the markup it names — the
    -- bodies wear `cpart' so the stylesheet needs no entry of its own — and
    -- native tabbing walks the sheet in exactly this order.  The list wraps
    -- markup rather than building it, the bodies being heterogeneous (labelled
    -- inputs, two selects, a box the server fills) and a builder for that shape
    -- being a template language this page has no use for: `SECTIONS' owns the
    -- headers and the order, the markup owns what is under them, and they join
    -- by id.  A `parts' id the markup does not carry throws here, at boot, which
    -- is where a join like that should fail.
  , "    const SECTIONS = ["
  , "      { title: \"general\", parts: [\"cgen\"] },"
  , "      { title: \"theme\", parts: [\"ctheme\"] },"
  , "      { title: \"keywords\", parts: [\"clayers\", \"ceff\", \"cfoot\"] },"
  , "    ];"
  , "    const csecs = el(\"csecs\");"
  , "    for (const s of SECTIONS) {"
  , "      const sec = part(csecs, \"div\", \"csec\");"
  , "      part(sec, \"div\", \"chdr\", s.title);"
  , "      for (const id of s.parts) sec.appendChild(el(id));"
  , "    }"
    -- The layers, and WHICH of them the one box is showing.  `crows' is the
    -- whole set with each layer's text in it — the on-screen box is a view of
    -- `crows[cat]' rather than the place the text lives, which is what makes a
    -- switch cost nothing.
  , "    let settings = false, crows = [], cat = 0;"
    -- The settings sheet's half of the pair the ladder drives ('subtreeSheet'
    -- is the other): the same four verbs, over the config layers and their own
    -- digests, filed under its own log scope.
  , "    const configSheet = {"
  , "      noteId: \"cnote\", scope: \"config\", state: \"synced\","
  , "      closed: \"settings closed — the files are as they were\","
  , "      dirty: () => cdirty(),"
  , "      flush: () => flushConfig(),"
  , "      refresh: () => config().then((b) => {"
  , "        for (const r of crows) {"
  , "          const fresh = (b.layers || []).find((l) => l.path === r.path);"
  , "          if (fresh) r.digest = fresh.digest;"
  , "        }"
  , "        return true;"
  , "      }),"
  , "      shut: () => shutSettings(),"
  , "    };"
    -- Claimed before the fetch, and refused over the other sheet.  `typing()'
    -- is not enough to keep the two apart: clicking the materialize sheet's own
    -- header blurs its textarea, and a `table' row is live again the moment it
    -- does — so the rule is stated here rather than left to the focus.
  , "    function openSettings() {"
  , "      if (activeSheet()) return;"
  , "      settings = true;"
  , "      config().then((b) => {"
  , "        if (!settings) return;   // an ESC arrived while the layers were out"
  , "        drawLayers(b);"
    -- The one field on the sheet whose value is this page's own, so it is drawn
    -- from storage rather than from the answer — and drawn on every open, which
    -- is what puts the preference back over a refused value left in the box.
  , "        el(\"clog\").value = logPref.get();"
  , "        cnote(\"synced\");"
  , "        el(\"config\").className = \"on\";"
    -- The top of the sheet, which is the general panel's first EDITABLE field
    -- (the default view above it is read-only, pinned from the table by `P'):
    -- the sheet opens where a reader can type, and Tab walks down from there.
  , "        el(\"ctarget\").focus();"
  , "      }).catch((e) => {"
  , "        settings = false;"
  , "        append(\"config\", \"error\", `settings failed: ${e.message}`);"
  , "      });"
  , "    }"
  , "    const config = () => getJSON(\"/config\");"
  , "    function drawLayers(b) {"
  , "      crows = (b.layers || []).map(layerRow).sort(byLayer);"
  , "      const pick = el(\"clayer\");"
  , "      pick.textContent = \"\";"
  , "      crows.forEach((r, i) => {"
  , "        const o = part(pick, \"option\", \"\", layerName(r));"
  , "        o.value = String(i);"
  , "      });"
  , "      showLayer(0);"
    -- The capture target is `system.org''s tree-wide LINE, bound to the system
    -- layer's row and out in its write: one file, one digest, one splice,
    -- wherever on the sheet it is drawn.  The FIRST
    -- system layer, which `/config' always serves and always leads with — a tree
    -- with none is a server that broke its own contract, and the throw lands in
    -- `openSettings''s catch as a settings failure rather than as a sheet that
    -- silently drops what a reader types.  The DEFAULT VIEW beside it is
    -- READ-ONLY here: composing a query belongs to the table's own widget —
    -- badges, completion, the grammar — and `P' pins the applied view as the
    -- default, so this field shows what is pinned and never rides a write.
  , "      const view = el(\"cfilter\"), cap = el(\"ctarget\");"
  , "      view.value = b.filter || \"\"; cap.value = b.capture || \"\";"
  , "      const sys = crows.find((r) => r.tag === null);"
  , "      sys.cap = cap; sys.capBase = cap.value;"
  , "      const kw = b.keywords || {};"
  , "      el(\"ceff\").textContent ="
  , "        `${(kw.active || []).join(\" \")} | ${(kw.inactive || []).join(\" \")}`;"
  , "    }"
    -- One layer, as this sheet holds it: where it is, what it was read as
    -- (`base'), what it says NOW (`text'), the digest a write is pinned to, and
    -- whatever the server last said about a write to it.  The text is the row's
    -- rather than a box's, which is the whole of what makes switching free.  The
    -- two tree-wide fields are the general panel's and are bound to the system
    -- layer by `drawLayers'; every layer carries the slots so one shape answers
    -- `cmoved' and one shape is posted.
  , "    const layerRow = (layer) => ({"
  , "      path: layer.path, tag: layer.tag, digest: layer.digest,"
  , "      base: (layer.lines || []).join(\"\\n\"),"
  , "      text: (layer.lines || []).join(\"\\n\"), err: \"\","
      -- The capture template is the layer's SECOND box and is kept the way the
      -- first one is: on the row rather than in the box, so switching layers
      -- costs no request and loses no edit.
  , "      tpl: layer.template || \"\", tplBase: layer.template || \"\","
  , "      view: null, viewBase: null, cap: null, capBase: null,"
  , "    });"
    -- SYSTEM FIRST, then the tags in their own alphabet.  The server's order is
    -- the walk's, which is where the directories turned up; a reader looking for
    -- one tag among forty wants the list they would guess at.  Two system layers
    -- keep the order they were served in — `sort' is stable — since nothing
    -- distinguishes them but the directory they came from.
  , "    const byLayer = (a, b) => (a.tag === null ? 0 : 1) - (b.tag === null ? 0 : 1)"
  , "      || String(a.tag).localeCompare(String(b.tag));"
      -- A tag layer is named in the grammar's own spelling — the same string
      -- a reader would type into the filter box.
  , "    const layerName = (r) => (r.tag ? `tag:${r.tag}` : \"system\");"
    -- The box is a VIEW of one layer, so the box's text goes back to the layer
    -- it came from before anything else reads or replaces it.  Every door does
    -- this first: a switch, a dirty check, a flush.
  , "    function takeLayer() {"
  , "      if (!crows[cat]) return;"
  , "      crows[cat].text = el(\"ctext\").value;"
  , "      crows[cat].tpl = el(\"ctpl\").value;"
  , "    }"
    -- What sits AROUND the box, and the only two things a write moves: the label
    -- carries the digest, so a layer this sheet just CREATED stops saying it is
    -- not there yet, and the line under it carries the layer's last refusal.
    -- Drawn on its own because a flush has to redraw both without touching the
    -- box the reader may still be typing in.
  , "    function showAround() {"
  , "      const r = crows[cat];"
  , "      el(\"clab\").textContent = r ? `${layerName(r)} · ${r.path}`"
  , "        + (r.digest ? \"\" : \" · not created yet\") : \"\";"
  , "      el(\"clerr\").textContent = r ? r.err : \"\";"
  , "    }"
  , "    function showLayer(i) {"
  , "      cat = Math.max(0, Math.min(i, crows.length - 1));"
  , "      el(\"clayer\").value = String(cat);"
  , "      el(\"ctext\").value = crows[cat] ? crows[cat].text : \"\";"
  , "      el(\"ctpl\").value = crows[cat] ? crows[cat].tpl : \"\";"
  , "      showAround();"
  , "    }"
    -- Switching layers is a READ, so it writes nothing and asks nobody: the text
    -- that was on screen goes back to its layer and the next one's comes out.  An
    -- edit outlives every switch, and the sync at the end writes all of them.
  , "    el(\"clayer\").addEventListener(\"change\", (e) => {"
  , "      takeLayer();"
  , "      showLayer(Number(e.target.value));"
  , "    });"
    -- `%' IN THE TEMPLATE BOX RAISES THE CODE LIST, which is this page's own
    -- value palette in its field mode and no widget of its own.  What it offers
    -- is the SERVER's list (`/capture' carries it beside the prompts), so the
    -- completion cannot come to offer a code the expansion does not know or
    -- omit one it does.  The `%' is not typed — committing writes the whole code
    -- and ESC writes nothing — and a literal one is the field's own line, since
    -- a line matching no entry commits as written (`freely').
  , "    el(\"ctpl\").addEventListener(\"keydown\", (e) => {"
  , "      if (keyName(e) !== \"%\") return;"
  , "      e.preventDefault();"
  , "      const box = el(\"ctpl\"), at = box.selectionStart, to = box.selectionEnd;"
  , "      askFrom(\"capture template · which code\","
  , "              CODES.map((c) => ({ label: c.code, hint: c.means, tag: c.code })),"
  , "              \"RET writes it · C-n/C-p walks · ESC leaves\","
  , "              (c) => insertCode(at, to, String(c.tag || \"\")));"
  , "    });"
    -- Back into the box at the caret it was raised from, and the model takes it:
    -- the palette blurred the textarea on its way up, so the selection this
    -- restores is the one the reader left rather than whatever the browser kept.
  , "    function insertCode(at, to, code) {"
  , "      const box = el(\"ctpl\"), text = box.value;"
  , "      box.value = text.slice(0, at) + code + text.slice(to);"
  , "      box.focus();"
  , "      box.setSelectionRange(at + code.length, at + code.length);"
  , "      takeLayer();"
  , "    }"
    -- The same four words the other sheet wears, through the same writer.
  , "    const cnote = (next, message) => note(configSheet, next, message);"
  , "    const cdirty = () => (takeLayer(), crows.some(cmoved));"
  , "    const cmoved = (r) => r.text !== r.base || r.tpl !== r.tplBase"
  , "      || (r.view !== null && r.view.value !== r.viewBase)"
  , "      || (r.cap !== null && r.cap.value !== r.capBase);"
    -- Every layer that moved, one POST each and each awaited — still one
    -- drift-locked write per FILE now that the boxes are one box.  A config file
    -- is its own write and its own lock, so one that drifted refuses on its own
    -- line while the rest land; there is no batch to roll back and none to want.
    -- A refusal is the LAYER's, and the sheet goes to the layer that has it: with
    -- one box on screen a message under it would otherwise describe a file the
    -- reader cannot see.  The FIRST refusal wins the selection, and the log names
    -- every one of them, since only one can be shown.
  , "    async function flushConfig() {"
  , "      takeLayer();"
  , "      cnote(\"syncing\");"
  , "      let ok = true, clashed = false, landed = -1;"
  , "      for (const r of crows) {"
    -- A layer this flush has nothing to send for carries no refusal either: its
    -- text is the file's again, so a message about the write that was refused
    -- describes an edit the reader has since taken back.
  , "        if (!cmoved(r)) { r.err = \"\"; continue; }"
  , "        // What was SENT, taken before the await: a keystroke landing while"
  , "        // the write is in flight would otherwise be marked as the file's"
  , "        // and never written, and the sheet would close on it silently."
  , "        const sent = r.text, tpl = r.tpl, view = r.view && r.view.value;"
  , "        const cap = r.cap && r.cap.value;"
  , "        const a = await postJSON(\"/config\","
  , "          { path: r.path, lines: sent.split(\"\\n\"),"
      -- The template is named only where it MOVED, which is the absent arm of
      -- the server's three-valued rule.  Sending it unconditionally would put
      -- every layer's own first heading back through the one-top-entry wall on
      -- every write, so a file whose heading is deeper than one — legal org,
      -- and no business of this box — could no longer have its cycle edited at
      -- all.  The two lines under it have kept this shape all along.
  , "            ...(tpl !== r.tplBase ? { template: tpl } : {}),"
  , "            ...(r.view ? { filter: view } : {}),"
  , "            ...(r.cap ? { capture: cap } : {}),"
  , "            digest: r.digest }).then(outcome)"
  , "          .catch((e) => ({ status: 0, body: { error: e.message } }));"
  , "        if (a.status === 200) {"
  , "          r.digest = a.body.digest; r.base = sent; r.tplBase = tpl; r.err = \"\";"
  , "          if (r.view) r.viewBase = view;"
  , "          if (r.cap) r.capBase = cap;"
  , "        } else {"
  , "          ok = false;"
  , "          if (a.status === 409) clashed = true;"
  , "          r.err = a.body.error || `sync failed (${a.status})`;"
  , "          if (landed === -1) landed = crows.indexOf(r);"
  , "          append(\"config\", \"error\", `${layerName(r)} · ${r.path}: ${r.err}`);"
  , "        }"
  , "      }"
    -- The BOX is left alone where nothing was refused: `C-x C-s' syncs mid-edit,
    -- so a reader typing while the write is in flight would have those keystrokes
    -- painted over by the text the flush snapshotted.  What is redrawn either way
    -- is what sits AROUND it — the label, since a created layer has a digest now,
    -- and the refusal line.  A landing takes the box under another `takeLayer',
    -- so the in-flight text goes home to its own layer before the swap.
  , "      if (landed === -1) showAround();"
  , "      else { takeLayer(); showLayer(landed); }"
  , "      cnote(ok ? \"synced\" : clashed ? \"conflict\" : \"error\");"
  , "      return ok;"
  , "    }"
    -- C-x C-s and the way out are the ladder's, over `configSheet': the
    -- refresh above is what a conflict overwrites under, and the close is
    -- pristine-costs-nothing, dirty-syncs-and-closes, trouble-discards.
  , "    function shutSettings() {"
  , "      el(\"config\").className = \"\"; settings = false; crows = []; cat = 0;"
  , "      configSheet.state = \"synced\";"
    -- And the keys go back to the table, in ONE place.  A control of the sheet
    -- holds the focus while it is up — which keeps the table's own keys dead
    -- under it — so the close is what has to give it up.  A browser drops the
    -- focus anyway when the box goes to `display:none'; saying it makes it the
    -- sheet's rule rather than a side effect, covering every control the sheet
    -- will ever hold rather than costing one `blur()' per control.
  , "      if (typing()) document.activeElement.blur();"
  , "    }"
  , "    // `/' summons the filter.  `openFilter' is the renderer's one entry point"
  , "    // for it whatever mode it is in — in palette mode it raises the overlay,"
  , "    // elsewhere it takes the box already on the page — so the shell asks for"
  , "    // it rather than reaching into the chrome.  An asset predating the call"
  , "    // has a resident box; focusing that is how this worked before."
  , "    const summons = () => can(table, \"openFilter\");"
  , "    const focusFilter = () => {"
  , "      if (summons()) { table.openFilter(); return; }"
  , "      const box = filterBox();"
  , "      if (box) { box.focus(); box.select(); }"
  , "    };"
  -- The one exception to keyboard-first, and the reason it is one: a coarse
  -- pointer has no `/' to press.  The chip row is the whole of the filter chrome
  -- a palette-mode page carries, so it doubles as the palette's button there —
  -- the same `focusFilter' the key runs, feature detection included.  Delegated
  -- from @#app@, so it survives every re-mount, and gated on the media query the
  -- rules are in, so a mouse sees nothing new.  A tap on a chip is that chip's
  -- own removal and stays the renderer's.
  , "    const coarse = () => typeof matchMedia === \"function\""
  , "      && matchMedia(\"(pointer: coarse)\").matches;"
  , "    el(\"app\").addEventListener(\"click\", (e) => {"
  , "      if (!coarse()) return;"
  , "      const t = e.target;"
  , "      if (!t.closest || !t.closest(\".tv-chips\") || t.closest(\".tv-chip\")) return;"
  , "      focusFilter();"
  , "    });"
  , "    // What a remount takes with it.  The table is `#app''s and goes when"
  , "    // the mount is replaced; the palette is the renderer's chrome inside it"
  , "    // and goes with it.  The sheet is a SIBLING of `#app' and survives by"
  , "    // where it sits, which is a fact about the layout rather than a promise"
  , "    // — so both are carried across by hand and neither depends on it."
  , "    let stashed = null;"
  , "    // The palette's lifecycle is the renderer's and this page does not reach"
  , "    // into its chrome past the field.  A field with focus is a palette the"
  , "    // reader is typing in; anything else is a query already committed, which"
  , "    // the URL is carrying anyway."
  , "    function typedFilter() {"
  , "      const box = filterBox();"
  , "      return box && document.activeElement === box ? box.value || \"\" : null;"
  , "    }"
  , "    function stash() {"
  , "      stashed = {"
  , "        // A PRISTINE SHEET NEEDS NOTHING: it is a sibling of `#app', so a"
  , "        // remount leaves it standing — both panes, the cursor in each, and"
  , "        // an open edit with them.  What cannot survive is a sheet that has"
  , "        // to be RE-READ, which is a dirty one: the reopen re-materializes"
  , "        // and rebuilds both panes, so the reader's work, the element they"
  , "        // were standing on and whatever an edit is holding all ride across."
  , "        sheet: editing && dirty()"
  , "          ? { id: editing.id, child: editing.child, raw,"
  , "              text: el(\"mtext\").value, props: props(), plan: planning(),"
  , "              at: drows[dat] ? drows[dat].id : null, col: dcol,"
  , "              open: openEditState(), digest: editing.digest }"
  , "          : null,"
  , "        palette: typedFilter(),"
  , "      };"
  , "    }"
    -- The open edit as three strings: which box, which element, and the text its
    -- fields are holding.  Null where nothing is open, which is the ordinary
    -- sheet — an edit is the one thing on this sheet a commit has not landed.
  , "    function openEditState() {"
  , "      if (!docOpen()) return null;"
  , "      const r = edit.row;"
  , "      return dparaing()"
  , "        ? { box: \"dpara\", id: r.id, kind: r.kind, key: \"\", val: el(\"dtext\").value }"
  , "        : { box: \"dedit\", id: r.id, kind: r.kind, key: el(\"dkey\").value,"
  , "            val: el(\"dval\").value };"
  , "    }"
  , "    function restore() {"
  , "      const was = stashed;"
  , "      stashed = null;"
  , "      if (!was) return;"
  , "      if (was.palette !== null) {"
  , "        focusFilter();"
  , "        // Assigning fires no input event, so the renderer is not asked to"
  , "        // complete or commit a query the reader has not finished typing."
  , "        const box = filterBox();"
  , "        if (box) { box.value = was.palette; box.focus(); }"
  , "      }"
  , "      if (was.sheet) reopen(was.sheet);"
  , "    }"
  , "    // The sheet, back open on what was in it — both panes, in the shape it"
  , "    // was showing.  The digest is re-asked for rather than carried over: a"
  , "    // file that moved while the mount was rebuilt is the conflict flow, and"
  , "    // flushing against a digest this page merely remembers is the silent"
  , "    // overwrite that flow exists to stop.  The reader's work is put back"
  , "    // either way — a restore never decides that an edit is worth less than"
  , "    // the file.  The baselines stay the file's, so what was dirty stays dirty."
  , "    function reopen(s) {"
  , "      headline(s.id, s.child).then((h) => {"
  , "        show(h, s.raw);   // which opens the sheet on the file as it now is"
  , "        el(\"mtext\").value = s.text;   // dirty again, against the file now"
  , "        if (!s.raw) {"
  , "          drawProps(s.props, s.plan);"
  , "          const back = drows.findIndex((r) => r.id === s.at);"
  , "          if (back !== -1) dat = back;"
  , "          dcol = s.col;"
  , "          drawDoc();"
  , "          if (s.open) reopenEdit(s.open);"
  , "        }"
  , "        if (h.digest !== s.digest) sync(\"conflict\");"
  , "      }).catch((e) => append(\"sync\", \"error\", `sheet restore failed: ${e.message}`));"
  , "    }"
    -- And the edit that was open, over the element it was open on.  A title cell
    -- belongs to the headline element rather than the model, so it is rebuilt
    -- from what was stashed; everything else is looked up by id and is gone where
    -- the file no longer holds it, a restore declining to invent a row.
  , "    function reopenEdit(o) {"
  , "      const r = o.kind === \"cell\""
  , "        ? { id: o.id, kind: \"cell\", key: o.key || \"title\", val: o.val }"
  , "        : drows.find((x) => x.id === o.id);"
  , "      if (!r) return;"
  , "      openEdit(o.box === \"dpara\" ? DPARA : DROW, r);"
  , "      el(o.box === \"dpara\" ? \"dtext\" : \"dval\").value = o.val;"
  , "      if (o.box === \"dedit\") el(\"dkey\").value = o.key;"
  , "    }"
  , "    // The one door that throws the mount away and builds a new one: a"
  , "    // `view-changed' close, and `g'.  Everything else that loses the socket"
  , "    // goes through `resync', which keeps the page it has."
      -- An archive's anchor belongs to the VIEW it was taken in, and both doors
      -- that replace one drop it: a mount thrown away here, and a new query in
      -- `commit'.  Without that, an anchor still armed when a `view-changed'
      -- close or `g' rebuilt the table would fire on the next socket frame and
      -- pull the cursor off the row the new view had just landed it on.
  , "    function remount(after) { leaving = arriving = null; stash(); start(after); }"
    -- `g': the view this tree configures, applied the way every other query is —
    -- written into the URL and asked of the server.  It goes through the mount
    -- because the chips are the renderer's and only a mount can be handed a query
    -- it did not commit itself; `start' then reads the URL this just wrote.
    -- Dropping onclose first stops the reconnect timer opening a second socket
    -- behind this one.  SEL is where the cursor should end up, which only a POP
    -- has an opinion about, every other caller leaving it out and taking the
    -- first row.  The landing rule lives HERE rather than in each caller, so a
    -- view applied through this door lands the same way whoever asked for it.
  , "    function applyView(b, q, landing, sel) {"
  , "      said(b, q ? `filter: ${JSON.stringify(q)}` : \"filter cleared\");"
  , "      if (socket) { socket.onclose = null; socket.close(); socket = null; }"
  , "      backoff = 1000;"
  , "      remember(q);"
  , "      remount((total) => { land(sel || null); if (landing) landing(total); });"
  , "    }"
    -- `g' is HOME rather than a step on the trail: it throws the crumbs away
    -- with the labels that named them.  Walking back out of a drill is DEL's,
    -- one rung at a time, where `g' is the door.
  , "    function applyDefault(b) {"
  , "      if (crumbing()) table.setCrumbs([]);"
  , "      crumbLabels = {};"
  , "      crumbSels = [];"
  , "      applyView(b, DEFAULT_QUERY);"
  , "    }"
    -- THE PIN: the applied query — sort tokens and all, since the order is the
    -- grammar's — becomes `system.org''s `#+GLANCE_DEFAULT_FILTER:' line,
    -- through the same drift-locked `/config' write the settings sheet rides.
    -- Composing stays the table's widget; nothing here parses the query.  The
    -- write reseeds and the reseed re-embeds `DEFAULT_QUERY' into the served
    -- page, so the pin is the next boot's view; a refusal — a 409 for a config
    -- edited elsewhere — is one `cmd' error line and nothing pinned.
  , "    function pinView(b) {"
  , "      const q = can(table, \"getQuery\") ? table.getQuery().trim() : \"\";"
  , "      getJSON(\"/config\").then((a) => {"
  , "        const sys = (a.layers || []).find((l) => !l.tag);"
  , "        if (!sys) { said(b, \"no system layer to pin into\"); return; }"
  , "        return postJSON(\"/config\","
  , "                        { path: sys.path, digest: sys.digest, filter: q })"
  , "          .then(() => {"
  , "            said(b, q ? `pinned · ${q}` : \"pinned · all rows\");"
  , "            append(\"config\", \"info\","
  , "                   `default view pinned: ${JSON.stringify(q)}`);"
  , "          });"
  , "      }).catch(failed(b, \"set-default-view\"));"
  , "    }"
    -- `@': the rows pointing AT the one at point.  A drill is a LOOK, so it
    -- takes the row at point and never the marked set — a mark is what a reader
    -- lays down to write over a run of rows, and inheriting it here would make
    -- every mark change what `@' means.
    --
    -- The crumb goes down BEFORE the view changes, so it records where the reader
    -- was standing rather than where they landed; `applyView' then writes both
    -- into the URL in one `remember'.  `ref:' is the server's own term
    -- (SCHEMA.md) — the renderer reads it as free text and would narrow further,
    -- which is why the drill re-fetches like every other query.  A drill out of
    -- the EMPTY query leaves no crumb, which is the absence of a special case
    -- rather than one: `all rows' IS the empty filter and DEL already lands
    -- there — the first rung strips the `ref:' token, the query goes empty, and
    -- with no trail behind it the key clears the filter, the very view the crumb
    -- would have restored — so the crumb, its label and its remembered row would
    -- be bookkeeping for a step the ladder takes anyway.  What goes with it is
    -- the cursor: DEL back out of that one drill lands on the first row like
    -- every other applied view, rather than on the row it was launched from.
    --
    -- ZERO REFERENCES IS NO JUMP, and the answer says so: the drill is PROBED
    -- first — the same query under `limit=1', a count and one row — and a total
    -- of nothing leaves the table, the filter and the trail exactly where they
    -- were.  A view with no rows is the one landing a reader cannot read anything
    -- off, and walking back out of it costs a keystroke to undo a keystroke; the
    -- probe costs a second fetch on a key that was already going to refetch,
    -- which is one keypress either way.
  , "    function relations(b) {"
  , "      const id = focusedId();"
  , "      if (!id) { said(b, \"no row\"); return; }"
  , "      if (!crumbing()) { said(b, \"this table-view.js has no crumbs\"); return; }"
  , "      const token = refToken(id), name = titleOf(id);"
  , "      load(`${asking(token)}&limit=1`).then((a) => {"
  , "        if (!a.total) {"
  , "          said(b, `no references to ${JSON.stringify(name)}`);"
  , "          append(\"cmd\", \"info\", `no references to headline ${JSON.stringify(name)}`);"
  , "          return;"
  , "        }"
  , "        drill(b, token, name);"
  , "      }).catch((e) => {"
  , "        if (e.name !== \"AbortError\") failed(b, \"relations\")(e);"
  , "      });"
  , "    }"
  , "    function drill(b, token, name) {"
  , "      if (query.trim()) {"
      -- The crumb records where the reader was STANDING: the query being left,
      -- and the row and column they were on, so walking back puts the cursor
      -- where it was rather than at the top of a view they had scrolled into.
  , "        const at = cells() ? table.getSelection() : null;"
  , "        const n = table.pushCrumb({ label: hereLabel(), query: query });"
  , "        crumbSels[n - 1] = at && at.id ? { id: at.id, col: at.col } : null;"
  , "        crumbSels.length = n;"
  , "      }"
  , "      crumbLabels[token] = `references of «${name}»`;"
  , "      applyView(b, token, (total) =>"
  , "        said(b, `references of ${JSON.stringify(name)} · ${total}`));"
  , "    }"
    -- `a' is the second canned view and the only one this page spells itself:
    -- the active rows carrying a date, which is `planned' — the virtual key over
    -- the two date cells, decidable by either side of the wire.  It is a VIEW
    -- rather than a mode, so `g' is the way home and every other key means what
    -- it always meant while it is applied.  The sort is part of the view —
    -- earliest first — so it is part of the QUERY: `sort:scheduled' says it to
    -- the server, which answers page one in that order, and to the renderer,
    -- which shows it on the header.  A canned view is one string again, where it
    -- used to be a query plus a call behind the answer, and `DEL' walks out of
    -- the order the way it walks out of the filter.
  , "    const AGENDA_QUERY = \"state:*active* -planned:*empty* sort:scheduled\";"
    -- What `a' says once its rows are on screen.  The count is the server's
    -- answer to the query, which is the one number a first page cannot give.
  , "    function landedAgenda(b, total) {"
  , "      said(b, `agenda · ${rowsWord(total)}`);"
  , "    }"
  , ""
  , "    // Keys.  The map is the JSON above — dispatch and echo read the one blob,"
  , "    // and there is one map: `n'/`j' both step a row, `f'/`l' both step a cell."
  , "    const MAPS = JSON.parse(el(\"keys\").textContent);"
  , "    // The theme selector.  `auto' is the media query — the attribute comes"
  , "    // off — and light and dark pin `data-theme' on the root, which is what"
  , "    // this page's own variables and the renderer's overrides both key off."
  , "    // The head has already applied the stored choice; this keeps the"
  , "    // control and the storage in step with it."
    -- A STORED PREFERENCE, and there are two.  DEF is what a reader gets back
    -- when nothing is stored and when storage is denied.  An EMPTIED value is a
    -- preference that is not there, so it is REMOVED rather than written as the
    -- empty string: what the reader asked for is the default, and a stored `""'
    -- would be a preference spelling one.
  , "    const pref = (key, def) => ({"
  , "      get() { try { return localStorage.getItem(key) || def; }"
  , "              catch (e) { return def; } },"
  , "      set(v) {"
  , "        try { if (v) localStorage.setItem(key, v);"
  , "              else localStorage.removeItem(key); } catch (e) { /* denied */ }"
  , "      },"
  , "    });"
  , "    const themed = pref(\"glance-theme\", \"auto\");"
  , "    function setTheme(name) {"
  , "      if (name === \"auto\") delete document.documentElement.dataset.theme;"
  , "      else document.documentElement.dataset.theme = name;"
  , "      themed.set(name);"
  , "      el(\"themesel\").value = name;"
  , "    }"
  , "    setTheme(themed.get());"
    -- The theme applies as it is picked and the sheet stays where it is: this is
    -- a preference rather than a write, so there is nothing to sync and no reason
    -- to close over it.  The select KEEPS the focus, the whole of the rule now
    -- that it lives inside a popup — a popup is a legitimate focus holder,
    -- `typing()' is true while one of its controls has the focus, and the table's
    -- own keys are dead underneath either way.  A control OUTSIDE a popup owed a
    -- hand-written `blur()' for that reason, and no such place is left here.
  , "    el(\"themesel\").addEventListener(\"change\", (e) => {"
  , "      setTheme(e.target.value);"
  , "      echo(`theme: ${e.target.value}`);"
  , "    });"
    -- THE LOG'S HEIGHT, the second preference this page keeps for itself.  The
    -- stylesheet owns the arithmetic and declares the default; what is written
    -- here is the NUMBER, onto the element, so a reader who never opens the
    -- sheet and a browser that refuses storage both get the same cap.
  , "    const LOG = { key: \"glance-log\", def: " <> T.pack (show logLinesDefault)
      <> ", min: " <> T.pack (show logLinesMin)
      <> ", max: " <> T.pack (show logLinesMax) <> " };"
    -- What TEXT asks for: a whole number inside the band, the DEFAULT when it
    -- names nothing at all, and `null' for everything else — a value this page
    -- declines rather than corrects, so a reader typing past the wall keeps the
    -- cap they had instead of watching it snap to it.  Half a number on the way
    -- to a whole one is the ordinary case of that.
  , "    const logLines = (text) => {"
  , "      const t = String(text).trim();"
  , "      if (!t) return LOG.def;"
  , "      return /^[0-9]+$/.test(t) && +t >= LOG.min && +t <= LOG.max ? +t : null;"
  , "    };"
  , "    const logPref = pref(LOG.key, \"\");"
  , "    const setLogLines = (n) =>"
  , "      el(\"log\").style.setProperty(\"--g-logn\", String(n));"
  , "    setLogLines(logLines(logPref.get()) || LOG.def);"
    -- Applied as it is typed, which makes the field a knob rather than a form:
    -- `input' rather than `change', a preference a reader has to leave the field
    -- to see being one they cannot aim.  Only a value this page takes is stored,
    -- so the box can hold a refused one and the next sheet-open draws the
    -- preference back over it.
  , "    el(\"clog\").addEventListener(\"input\", (e) => {"
  , "      const n = logLines(e.target.value);"
  , "      if (n === null) return;"
  , "      logPref.set(String(e.target.value).trim());"
  , "      setLogLines(n);"
  , "      echo(`log: ${n} lines`);"
  , "    });"
  , ""
  -- The resident key line, under the log: what can run, where the echo pill says
  -- what just did.  The table is the blob's ('Glance.Web.Keymap.keyHints'), naming commands rather
  -- than keys, so the spelling comes out of the same rows the dispatch reads.  A
  -- command two keys spell shows the FIRST of them, the order 'Glance.Web.Keymap.keyBindings' lists
  -- it in.
  , "    function hints() {"
  , "      const seq = (command) => {"
  , "        const b = MAPS.rows.find((x) => x.command === command && x.scope === \"table\");"
  , "        return b && b.handler ? b.seq : null;   // a staged row is no offer"
  , "      };"
  , "      el(\"kbd\").textContent = MAPS.hints"
  , "        .map((h) => [h.commands.map(seq).filter(Boolean), h.label])"
  , "        .filter(([keys]) => keys.length)"
  , "        .map(([keys, label]) => `${keys.join(\"/\")} ${label}`)"
  , "        .join(\" · \");"
  , "    }"
  , "    hints();"
  , "    const NAMED = { Enter: \"RET\", Tab: \"TAB\", \" \": \"SPC\", Escape: \"ESC\","
  , "      Backspace: \"DEL\", Delete: \"<delete>\", ArrowUp: \"<up>\", ArrowDown: \"<down>\","
  , "      ArrowLeft: \"<left>\", ArrowRight: \"<right>\", Home: \"<home>\", End: \"<end>\","
  , "      PageUp: \"<prior>\", PageDown: \"<next>\" };"
    -- ONE NAME PER PRESS, and the SPLIT inside it is the whole layout rule.
    -- Every listener on this page names a key through here, so the dispatch, the
    -- sheet, the palette and the popups inherit one spelling of it.
    --
    -- A LETTER IS A PHYSICAL KEY.  `e.code' says where a key SITS — `KeyN' is
    -- the one a Latin layout writes `n' on — so a reader on Cyrillic navigates
    -- with `т з о л' and archives with `в', the letters landing where the
    -- fingers already are.  SHIFT IS THE UPPERCASE BINDING rather than an `S-'
    -- modifier, which keeps `d' and `D' the two rows they are; a chord comes
    -- through the same door, so the `C-t' completing `C-c' is the physical key
    -- too.  Shift ALONE decides the case, so a held CapsLock lands on the
    -- lowercase row — the safe half of that pair, `d' flagging where `D' writes.
    --
    -- EVERYTHING ELSE IS THE CHARACTER, `e.key' as it always was: the named
    -- keys, the function keys, and the PUNCTUATION.  `^ : + < > [ ] / , ! @'
    -- sit at different positions on every layout, so there is no position to
    -- bind and the character is the honest answer; a press carrying no `code' at
    -- all falls back to it whole.
    --
    -- THE CODE WINS ON EVERY `KeyA'–`KeyZ' PRESS, whatever the layout writes on
    -- it: the map is QWERTY'S POSITIONS.  Two consequences, both named rather
    -- than worked around.  A Latin layout that MOVES its letters reads its own
    -- labels as this map's — an AZERTY `a' sits on `KeyQ', so it is `q' here,
    -- and a Dvorak hand likewise.  And a layout spelling NO `<' or `[' (the
    -- Russian one does not) cannot reach the punctuation half; the letters carry
    -- movement, marks, states and the archive, and the rest wants a layout that
    -- has the character.
  , "    const LETTER = /^Key([A-Z])$/;"
  , "    function keyName(e) {"
  , "      let base = NAMED[e.key], special = base !== undefined;"
  , "      if (!special && /^F\\d{1,2}$/.test(e.key))"
  , "        { base = `<${e.key.toLowerCase()}>`; special = true; }"
  , "      if (!special) {"
  , "        const sits = LETTER.exec(e.code || \"\");"
  , "        base = sits ? (e.shiftKey ? sits[1] : sits[1].toLowerCase()) : e.key;"
  , "        if (base.length !== 1) return null;"
  , "      }"
  , "      let mods = \"\";"
  , "      if (e.ctrlKey) mods += \"C-\";"
  , "      if (e.altKey || e.metaKey) mods += \"M-\";"
  , "      if (special && e.shiftKey) mods += \"S-\";"
  , "      return mods + base;"
  , "    }"
  , "    let echoAt = null, pending = [], pendingAt = null;"
  , "    function echo(text, hold) {"
  , "      const pill = el(\"echo\");"
  , "      pill.textContent = text;"
  , "      pill.style.opacity = \"1\";"
  , "      clearTimeout(echoAt);"
  , "      if (!hold) echoAt = setTimeout(() => (pill.style.opacity = \"0\"), 1500);"
  , "    }"
  , "    function prefix(keys) {"
  , "      pending = keys;"
  , "      clearTimeout(pendingAt);"
  , "      if (!keys.length) return;"
  , "      const shown = keys.join(\" \");"
  , "      echo(`${shown} -`, true);"
  , "      pendingAt = setTimeout(() => { pending = []; echo(`${shown} - timed out`); }, 2000);"
  , "    }"
    -- THE MODAL SURFACES, as ONE list.  Each holds the keys with NOTHING
    -- FOCUSED — the subtree sheet's two panes, the settings sheet's chrome, the
    -- value palette in letter mode, and the two popups, which browse on the
    -- table's own movement keys and write on its own `d'/`D'/`u' — so each would
    -- otherwise leave the table's keys live underneath it, `d' included.
    --
    -- TWO LAYERS RATHER THAN A STACK.  A SHEET is a WORKSPACE: it stands, it
    -- holds a cursor, a reader works in it, and there are two — the subtree's
    -- and the settings', never both, `openSettings' refusing over an open sheet.
    -- A MOMENTARY is raised OVER one to answer a question — the value palette,
    -- the tags popup, the link popup — and is answered and gone.  At most one
    -- momentary is ever up, opening one closing whichever stood, at the DOOR
    -- rather than in the listeners (`sole'); the open one holds the keys
    -- unconditionally, every reader asking `momentary()'; and closing it gives
    -- them back to the sheet with its cursor exactly where it was.  A rank could
    -- express none of that, the sheet being also the thing that raises a
    -- momentary over itself.
    --
    -- ORDER DECIDES ONE THING, and only through `momentary()': `+' over the tags
    -- popup leaves BOTH `prompt' and `tags' up — the field is that popup's own,
    -- which is what `sole''s `keep' exempts — and the tie goes to the earlier
    -- entry.  Swapping those two hands the add field's letters to the tags
    -- listener.
    --
    -- FOUR READERS: `typing()' asks whether ANY surface is up, which kills every
    -- `table' row; `live''s `modal' arm asks whether a WORKSPACE is;
    -- `momentary()' asks which of the raised ones is; and `cancel' walks the list
    -- for the surface ESC belongs to.  Each entry names its own `up', the `off'
    -- that closes it, and the OPEN EDIT that is a rung under it — the panel's
    -- row, the popups' overlays — which ESC puts back before the surface itself
    -- hears the key.  NEITHER SHEET names an `off': ESC from one falls through to
    -- the sheet ladder below, where closing a workspace belongs.
  , "    const SURFACES = ["
  , "      { name: \"prompt\", momentary: true, up: () => !!prompting, off: unask },"
  , "      { name: \"capture\", momentary: true, up: capUp, off: shutCapture },"
  , "      { name: \"links\", momentary: true, up: linking, off: shutLinks,"
  , "        edit: lediting, shut: cancelLinkEdit },"
  , "      { name: \"tags\", momentary: true, up: managing, off: shutTags,"
  , "        edit: renaming, shut: cancelRename },"
  , "      { name: \"sheet\", up: docHolds, edit: sheetOpen, shut: cancelSheetEdit },"
      -- The settings sheet, the second WORKSPACE and the fifth surface.  It
      -- names no `off': ESC from it falls through to the sheet ladder below,
      -- where `activeSheet' already answers for both.  Joining the list is what
      -- makes `typing()' see it — an omitted surface leaves every `table' row
      -- live underneath, `d' among them, and a click on this sheet's own chrome
      -- blurs the field the focus branch was catching it by.
  , "      { name: \"config\", up: () => settings },"
  , "    ];"
    -- WHICH momentary is up, and there is at most one.  Read off the list, so a
    -- fourth is one entry and every reader has it at once.
  , "    const momentary = () =>"
  , "      (SURFACES.find((s) => s.momentary && s.up()) || {}).name || null;"
    -- THE ONE DOOR EXCLUSIVITY IS SPELLED AT.  Every raise passes through here,
    -- so "at most one" is a property of the doors rather than a rule the
    -- listeners have to keep between them.
    --
    -- KEEP is the one exemption, a field a surface raises for ITSELF rather than
    -- a stacking pair: `+' over the tags popup raises the palette as THAT POPUP'S
    -- OWN FIELD, and the popup is what the typed name goes back into, so it is no
    -- second momentary competing with it.  `SURFACES'' order is load-bearing for
    -- exactly that pair, through `momentary()', and is stated there. WALKED OFF
    -- `SURFACES' rather than restated: a fourth momentary is one entry there and
    -- this closes it without an edit, where a hand-written list was a fourth
    -- registration site whose omission failed silently.
  , "    function sole(keep) {"
  , "      if (keep) return;"
  , "      for (const s of SURFACES) if (s.momentary && s.up()) s.off();"
  , "    }"
  , "    // A focus that keeps its own keys: the filter box, the sheet, and the"
  , "    // keys select, which navigates on the arrows this map would otherwise"
  , "    // take for row movement — and the modal surfaces, which hold them with"
  , "    // nothing focused at all."
  , "    const typing = () => {"
  , "      const a = document.activeElement;"
  , "      return SURFACES.some((s) => s.up())"
  , "        || (!!a && (a.tagName === \"INPUT\" || a.tagName === \"TEXTAREA\""
  , "                     || a.tagName === \"SELECT\" || a.isContentEditable));"
  , "    };"
    -- `modal' is "a WORKSPACE is up", which is every non-momentary surface:
    -- the subtree sheet and the settings sheet.  Never both — `openSettings'
    -- refuses over an open sheet, which keeps `C-x C-s' and `ESC' from guessing
    -- which one they meant.  Read off `SURFACES' rather than naming the two, so
    -- a third workspace is one entry there and this arm has it at once.
  , "    const live = (b) => b.scope === \"any\""
  , "      || (b.scope === \"modal\" && SURFACES.some((s) => !s.momentary && s.up()))"
  , "      || (b.scope === \"table\" && !typing());"
  , "    // A live selection means C-c and C-x are copy and cut, and the browser"
  , "    // decides that on this keydown — so the prefix does not claim them."
  , "    function selecting() {"
  , "      const a = document.activeElement;"
  , "      if (a && typeof a.selectionStart === \"number\")"
  , "        return a.selectionStart !== a.selectionEnd;"
  , "      const s = document.getSelection();"
  , "      return !!s && !s.isCollapsed;"
  , "    }"
    -- ONE IMPLEMENTATION, TWO KEYS.  `U' clears the marks AND the flags; `DEL'
    -- clears the MARKS ALONE, since a flag is the archive queue and a backspace
    -- must not empty it.  Both speak `unmark-all', the command's own name, so a
    -- reader who learns it off either pill can type it back.  The two answers
    -- differ over NOTHING TO CLEAR: `U' is the key for this and says so on an
    -- asset that has no marks, where `DEL' is a LADDER whose rung that finds
    -- nothing has to fall through to the next one silently.  Hence the boolean —
    -- "did this key spend its press", which only `DEL' reads.
  , "    function clearMarking(b, alsoFlags) {"
  , "      if (!marking()) {"
  , "        if (alsoFlags) said(b, \"this table-view.js has no marks\");"
  , "        return false;"
  , "      }"
  , "      const n = table.markedCount();"
  , "      if (!n && !alsoFlags) return false;"
  , "      table.clearMarks();"
  , "      if (alsoFlags && flagging()) table.clearFlags();"
  , "      said(b, alsoFlags ? \"all marks and flags cleared\" : String(n));"
  , "      return true;"
  , "    }"
    -- A binding wearing another command's NAME, for the one key that delegates:
    -- `DEL' really does run `unmark-all', so the pill has to say so, and the
    -- echo rule is that the slot after the arrow is the function that ran.
  , "    const named = (b, command) => ({ seq: b.seq, command });"
  , "    const HANDLERS = {"
  , "      nextRow: () => move(1),"
  , "      previousRow: () => move(-1),"
  , "      nextColumn: (b) => moveCol(b, 1),"
  , "      previousColumn: (b) => moveCol(b, -1),"
  , "      nextPage: (b) => turnPage(b, 1),"
  , "      previousPage: (b) => turnPage(b, -1),"
  , "      firstRow: (b) => endStop(b, false),"
  , "      lastRow: (b) => endStop(b, true),"
    -- `^' sorts by the column the CELL selection is standing in, which is the
    -- whole of how it picks one: a whole-row selection names no column, and
    -- guessing one — the primary, the first, the last one sorted — would be this
    -- page inventing a rule the renderer's own `^' does not have (there the
    -- answer is where point is), so it refuses and says which key picks a column.
    --
    -- `sortable' is the RENDERER's opt-in and `sortBy' ignores it — the flag
    -- gates what a reader may reach, where a producer's own call is the
    -- producer's business — so a page driving a reader's key has to honour it
    -- here or it would sort a column the header click will not.
    --
    -- `^' PROMOTES: the column at point becomes the chain's head ascending (the
    -- rest shift down, deduped); on the column already leading it flips that key
    -- alone.  Composing a chain = pressing over columns in reverse priority
    -- order — the web's spelling of table-view.el's C-u ^.
    --
    -- IT IS A QUERY EDIT.  The renderer writes the new chain into the applied
    -- query as ONE arrow-form `sort:' token and delivers it, so the press lands
    -- here as an ordinary filter commit: the rows in hand re-order at once, the
    -- URL is rewritten, the server is asked for that order and answers page one
    -- in it, and DEL walks the keys back off one at a time, the chain being one
    -- chip whose last key the renderer gives up per press.  Nothing on this page
    -- remembers a sort.
  , "      toggleSort: (b) => {"
  , "        if (!sorts()) { said(b, \"this table-view.js has no sort\"); return; }"
  , "        const at = column(), c = at === null ? null : cols[at];"
  , "        if (!c) { said(b, \"no column selected — f/l to pick one\"); return; }"
  , "        const named = c.header || c.key;"
    -- `sortable' is the renderer's opt-in and `sortPromote' is where it is
    -- enforced, so the refusal is READ OFF the call rather than derived a
    -- second time here — the key still has to SPEAK it.
  , "        if (!table.sortPromote(c.key)) { said(b, `${named} does not sort`); return; }"
  , "        const chain = table.getSort() || [], head = chain[0];"
  , "        said(b, head ? `${named} ${head.ascending !== false ? \"▲\" : \"▼\"}`"
      <> " + (chain.length > 1 ? ` · ${chain.length} keys` : \"\") : named);"
  , "      },"
  , "      materializeRow: () => {"
  , "        const id = focusedId();"
  , "        if (id) materialize(id);"
  , "        else append(\"cmd\", \"info\", \"no row focused — n or p picks one\");"
  , "      },"
  , "      markToggle: (b) => mark(b, true),"
  , "      unmarkRow: (b) => mark(b, false),"
  , "      unmarkAll: (b) => clearMarking(b, true),"
    -- `M' marks the whole loaded set, which is the renderer's call because the
    -- set is the renderer's: a page it is not showing is still marked.
  , "      markAll: (b) => {"
  , "        if (!marking() || !can(table, \"markAll\"))"
  , "          { said(b, \"this table-view.js has no mark-all\"); return; }"
  , "        table.markAll();"
  , "        said(b, `marked · ${table.markedCount()}`);"
  , "      },"
    -- dired's `d' at the table, the gesture `flagKey' holds.  The command is in
    -- ONCE, so a HELD `d' delivers exactly one press and can never flag and
    -- archive from one keystroke.  `u' takes a flag off, through `mark'.
  , "      archiveFlag: (b) => flagKey(\"d\", XFLAGS(b), (what) => said(b, what)),"
  , "      priorityUp: (b) => cyclePriority(b, 1),"
  , "      priorityDown: (b) => cyclePriority(b, -1),"
  , "      applyDefault, pinView, relations, focusFilter, toggleRaw, openSettings,"
    -- One `save-buffer' over two sheets: `saveSheet' asks `activeSheet' which
    -- is up, so there is nothing to choose between here.
  , "      save: saveSheet,"
  , "      commitEdit: (b) => { if (docOpen()) commitDocEdit(b);"
  , "                           else said(b, \"nothing open here\"); },"
    -- D is dired's key and org-glance's `delete', and it is the same gesture
    -- with no flagging step in front of it — the same call the second `d' makes,
    -- differing in the key it hands over and so in the name the echo spells.
  , "      archiveRows: (b) => flagKey(\"D\", XFLAGS(b), (what) => said(b, what)),"
    -- C-c C-t asks which state, over whatever the command would run on — the
    -- marked set, else the row at point.  The asking is `askState''s, shared with
    -- the sheet's own `t'; what this key decides is WHICH ROWS.
  , "      setState: (b) => overTargets(b, \"set state\", askState),"
    -- `:' is the agenda's own key for the same question, over the same rows.  It
    -- raises the POPUP, which STAYS up under every write it carries: managing
    -- tags is several ops over one set where setting a state is one, and closing
    -- after each would make the second op a fresh press and a fresh resolution.
  , "      manageTags: (b) => overTargets(b, \"tags\", askTags),"
    -- `+' is a CHAIN of prompts and nothing else: which tag, whatever that tag's
    -- template asks, then the line.  What it collects goes straight to the
    -- server, which knows the file and holds the template.
  , "      capture: (b) => openCapture(b),"
    -- `o' FOLLOWS the row, and how many links it holds decides the whole gesture:
    -- none is a refusal, one opens, several raise the popup.  The count is the
    -- server's answer, so the popup can only go up behind the request, which is
    -- why this one is raised late where the state palette is raised on the press;
    -- by then the `o' that asked has been dispatched and gone, so nothing is
    -- travelling and no press is declined.  One consequence, named rather than
    -- worked around: the popup is also where a link is EDITED, so a row holding
    -- exactly ONE link is followed and never listed, and that link has no editor.
    -- Following is what this key promises, and a list of one to pick from would
    -- be chrome over every press that meant to open something.  A key that LISTS
    -- whatever the count is would settle it.
  , "      openLinks: (b) => {"
  , "        const id = focusedId();"
  , "        if (!id) { said(b, \"no row\"); return; }"
  , "        linksOf(id).then((a) => followLinks(b, id, a, a.links || []))"
  , "          .catch(failed(b, \"open\"));"
  , "      },"
  , "      applyAgenda: (b) => applyView(b, AGENDA_QUERY, (total) => landedAgenda(b, total)),"
  , "      schedulePlan: (b) => planRows(b, \"SCHEDULED\"),"
  , "      deadlinePlan: (b) => planRows(b, \"DEADLINE\"),"
    -- `q' is the SUBTREE sheet's door alone, which is why it asks after
    -- `editing' rather than after whichever sheet is up.
  , "      quitWindow: () => (editing ? leaveSheet()"
  , "        : append(\"cmd\", \"info\", \"q closes the sheet; there is no window to quit\")),"
    -- ONE KEY OUT OF WHICHEVER OVERLAY IS UP — the prompt first, being the one
    -- that can be raised over an open sheet — walked off `SURFACES' rather than
    -- restated as a chain of tests: each surface's OPEN EDIT is the rung under
    -- it, so ESC puts a panel row, a link or a tag back and only the next press
    -- reaches the surface holding it.  The sheet is the floor — the panel names
    -- no `off', so ESC from nav falls through to it — and a stray focus is what
    -- is left under that.  The surfaces are mutually exclusive in practice (each
    -- is raised from a table key, and `typing()' has already killed every one of
    -- those by the time another is up), so the ORDER decides nothing a reader can
    -- reach; it is the list's, and the list's order is the listeners'.
  , "      cancel: () => {"
  , "        for (const s of SURFACES) {"
  , "          if (s.edit && s.edit()) { s.shut(); return; }"
  , "          if (s.off && s.up()) { s.off(); return; }"
  , "        }"
  , "        if (activeSheet()) leaveSheet();"
  , "        else if (typing()) document.activeElement.blur();"
  , "      },"
  , "      // The filter's own backspace: the renderer drops the token and the"
  , "      // shell follows it — one commit, one URL, focus left on the table."
  , "      //"
  , "      // A LADDER, in three rungs, and the rhyme is the backspace's: ERASE"
  , "      // THE LAST STRUCTURE STANDING.  A MARKED SET is one, so while there"
  , "      // are marks DEL takes them off and stops — the marks alone, since a"
  , "      // FLAG is the archive queue and a backspace must not empty it.  Then"
  , "      // the query's last TOKEN, as it always has.  Then, when the strip"
  , "      // EMPTIES the query and there is a trail behind it, the same key walks"
  , "      // back out of the drill that built the view — it applies the crumb's"
  , "      // query INSTEAD of the empty one, so `@' and `DEL' are one step out"
  , "      // and one step back rather than a step and a half.  A rung with"
  , "      // nothing under it falls through in silence; only the rung that RUNS"
  , "      // speaks."
  , "      filterDrop: (b) => {"
  , "        if (clearMarking(named(b, \"unmark-all\"), false)) return;"
  , "        if (!strips()) { said(b, \"this table-view.js has no filter tokens\"); return; }"
  , "        if (!table.stripLastToken()) { said(b, \"no filter\"); return; }"
  , "        const left = table.getQuery().trim();"
  , "        if (!left && crumbing() && trail().length) {"
  , "          // The row this crumb was pushed from, when the side table is"
  , "          // still in step with the trail the renderer is holding."
  , "          const sel = selsFit() ? crumbSels.pop() : null;"
  , "          const back = table.popCrumb();"
  , "          // The view being left takes its label with it; a crumb further"
  , "          // down the trail keeps its own, since the map is keyed by token."
  , "          delete crumbLabels[query];"
  , "          applyView(b, back.query, () => said(b, `back to ${back.label}`), sel);"
  , "          return;"
  , "        }"
  , "        commit(left);"
  , "        said(b, left ? `filter: ${JSON.stringify(left)}` : \"filter cleared\");"
  , "      },"
  , "    };"
  , "    // The row is handed to its handler: one that names what it landed on"
  , "    // — the filter left, the column arrived at — echoes over this line with"
  , "    // the same `seq → command' opening."
  , "    function run(b) {"
  , "      echo(`${b.seq} → ${b.command}${b.help ? ` · ${b.help}` : \"\"}`);"
  , "      const handler = b.handler && HANDLERS[b.handler];"
  , "      if (handler) handler(b);"
  , "      else append(\"cmd\", \"info\","
      <> " `${b.seq} (${b.command}) — arrives with daemon commands (M4)`);"
  , "    }"
  , "    document.addEventListener(\"keydown\", (e) => {"
      -- A KEY ANOTHER LISTENER HAS ALREADY CLAIMED IS NOT THIS MAP'S, and the
      -- document is the one surface that can hand a key back mid-press: its
      -- listener runs AHEAD of this one, and `DEL' there closes the sheet — so by
      -- the time this ran, `typing()' had gone false and the table's own `DEL'
      -- would strip a filter token off the view underneath.  `defaultPrevented'
      -- is the DOM's own word for handled, what every listener on this page
      -- already says by calling `preventDefault'; the three that run BEHIND this
      -- one are unaffected, since a row it claims it also runs.
  , "      if (e.defaultPrevented) return;"
  , "      const k = keyName(e);"
  , "      if (!k) return;"
  , "      const keys = pending.concat([k]);"
  , "      const here = MAPS.rows.filter(live);"
  , "      // A row is in play while its keys open with the ones typed so far."
  , "      const opens = (b) => keys.every((key, i) => b.keys[i] === key);"
  , "      const hit = here.find((b) => b.keys.length === keys.length && opens(b));"
  , "      // A held key still belongs to this map — it is claimed either way —"
  , "      // but a destructive one runs once per press."
  , "      if (hit) {"
  , "        prefix([]);"
  , "        e.preventDefault();"
  , "        if (!(e.repeat && MAPS.once.indexOf(hit.command) !== -1)) run(hit);"
  , "        return;"
  , "      }"
  , "      if (here.some((b) => b.keys.length > keys.length && opens(b))) {"
  , "        if (!selecting()) { e.preventDefault(); prefix(keys); }"
  , "        return;"
  , "      }"
  , "      if (!pending.length) return;   // not ours; the browser keeps it"
  , "      prefix([]);"
  , "      if (MAPS.reserved.indexOf(k) === -1) e.preventDefault();"
  , "      echo(`${keys.join(\" \")} is undefined`);"
  , "    });"
    -- The prompt's own keys, behind the dispatch above and safe for the reason
    -- stated at the sheet's listener.  C-n and C-p are reserved chords the map
    -- never claims, and claiming them HERE is the palette's business rather than
    -- the map's — the same way a focused select keeps its arrows.
    --
    -- Letter mode is bare letters only: `keyName' spells a chord `C-t' and a held
    -- shift `T', neither a claimed letter, so both fall through to whatever else
    -- wants them.  `keyName' names the press here too, so the which-key letters
    -- are PHYSICAL keys the way the map's are — the pool is a-z by construction
    -- (`whichKeys'), and a Cyrillic press arrives already spelled in that
    -- alphabet.
    --
    -- `raising' AND EXCLUSIVITY ARE DIFFERENT RULES, which is why `sole' does not
    -- absorb it: exclusivity is one surface closing ANOTHER at the door, where
    -- `raising' is this surface declining the one keydown that RAISED it — `t' is
    -- both the opener and a letter in what it opens, and this listener sits
    -- behind the dispatch, so that press arrives here next.  Only one surface is
    -- involved, so no ordering between surfaces could say anything about it.
  , "    document.addEventListener(\"keydown\", (e) => {"
  , "      if (!prompting) return;"
  , "      if (prompting.raising) { prompting.raising = false; return; }"
  , "      const k = keyName(e);"
    -- A bare modifier spells no key, and an unbound entry claims no letter:
    -- without this the two nulls would meet and Shift would commit whatever
    -- came out of the pool empty.
  , "      if (!k) return;"
    -- The mode that holds a LINE rather than a list (`askText'): RET takes the
    -- line as typed and every other key is the field's own, with nothing to
    -- narrow and no letter to commit.  A palette whose typing reaches past its
    -- list takes the line as an ENTRY (`freely'), one with no list as text.
  , "      if (prompting.text) {"
  , "        if (k !== \"RET\") return;"
  , "        takeChoice(freely() || { text: el(\"pinput\").value });"
  , "        e.preventDefault();"
  , "        return;"
  , "      }"
    -- A letter writes, so it runs once per press — the `ONCE' rule, owed here
    -- rather than by the map because the key that OPENS this palette is a letter
    -- too, and a held one would raise it and commit through it.  The repeat is
    -- claimed either way, the way the dispatch claims one it declines to run.
    -- DEL arrives here as an ordinary entry key, `*empty*' holding it as its own;
    -- a palette with no such entry — the tag one — leaves the press to nobody,
    -- `typing()' having already killed the map's own DEL.
  , "      if (!prompting.narrow) {"
  , "        const hit = prompting.choices.find((c) => c.key === k);"
        -- The fallback's own foot, named where the fallback is entered: the
        -- letters are gone and the field's keys take their place.
  , "        if (k === \"/\")"
  , "          fieldMode(\"RET sets it · C-n/C-p walks · ESC leaves\");"
  , "        else if (!hit) return;"
  , "        else if (!e.repeat) takeChoice(hit);"
  , "        e.preventDefault();"
  , "        return;"
  , "      }"
  , "      const step = k === \"<down>\" || k === \"C-n\" ? 1"
  , "                 : k === \"<up>\" || k === \"C-p\" ? -1 : 0;"
  , "      if (step) walkChoices(step);"
  , "      else if (k === \"RET\") takeChoice(prompting.shown[prompting.at] || freely());"
  , "      else return;"
  , "      e.preventDefault();"
  , "    });"
    -- A MOMENTARY POPUP'S KEYS, the two that BROWSE A MOUNT sharing the whole
    -- shape: stand down unless this surface is the one up, name the key, hand it
    -- to the open EDIT where there is one, then row movement, then the popup's
    -- own chain, and claim whatever landed.  Written once, so a third popup is a
    -- declaration rather than a fourth listener to keep in step.  Registered
    -- BEHIND the dispatch, safe for the value palette's reason: while a popup is
    -- up `typing()' has already made every `table' row dead, so the only row that
    -- can have fired ahead is `ESC' — which is the one that should, `cancel'
    -- closing whichever overlay is up.
    --
    -- TWO ASYMMETRIES, both declared rather than flattened away.
    -- `defaultPrevented': a key another listener has already CLAIMED is not this
    -- one's — the tags popup can have a field raised over it (`+'), whose
    -- listener runs ahead of this and closes the overlay as it commits, so the
    -- very `RET' that added a tag would arrive here and open the rename.  It is
    -- asked of BOTH now, the link popup raising no field today and "handled is
    -- handled" being a rule no one surface should keep for itself.  `e.repeat':
    -- a key that WRITES runs once per press, spelled in the chain that owns it
    -- rather than lifted here — the tags popup's `d'/`D'/`u' are the deletion
    -- gesture, where a repeat that survived would flag a tag and remove it from
    -- ONE press.
  , "    function popupKeys(name, mount, o) {"
  , "      document.addEventListener(\"keydown\", (e) => {"
  , "        if (momentary() !== name || e.defaultPrevented) return;"
  , "        const k = keyName(e);"
  , "        if (!k) return;"
  , "        if (o.editing()) { if (!o.editKeys(k, e)) return; }"
  , "        else {"
  , "          const step = rowStep(k);"
  , "          if (step) stepIn(mount(), step);"
      -- DEL ERASES THE LAST STRUCTURE STANDING, the backspace's rhyme everywhere
      -- on this page: over the table it takes the marks, then the query's last
      -- token, then a rung off the drill trail.  Over a popup the popup IS that
      -- structure — neither of these has an inner ladder — so the key closes it,
      -- through the same `off' ESC reaches and read off `SURFACES' rather than
      -- named a second time here.  IN NAV ALONE: inside an open edit the key is
      -- the FIELD's own erase, the edit branch above declining it, and a key this
      -- listener declines is one it does not `preventDefault', which is the whole
      -- of what leaves it to the field.
  , "          else if (k === \"DEL\") {"
  , "            (SURFACES.find((s) => s.name === name) || {}).off();"
  , "            keySaid(k)(\"keyboard-quit\");"
  , "          }"
  , "          else if (!o.keys(k, e)) return;"
  , "        }"
  , "        e.preventDefault();"
  , "      });"
  , "    }"
    -- MOVE, LOOK, OPEN — the whole of the link popup today.  Row movement is
    -- `rowStep', the property panel's own: both spellings and the arrows, bound
    -- unconditionally the way the panel's are, the popup holding no field and
    -- every printable key being free.
    --
    -- `o' is the OPEN key, the key that raised this — the table's own `o' carried
    -- inside, over the link the cursor is on rather than over the row.  It opens
    -- and CLOSES, both outcomes alike (the tab and the refusal), picking one link
    -- being what the popup was raised to do and a popup that stayed up on the
    -- refusal being a second rule for the same key.
    --
    -- `RET' EDITS the link at point in place — the row's own title and url cells
    -- becoming fields over themselves, `TAB' between them, `RET' committing and
    -- `ESC' restoring, which is the property panel's edit model exactly.  ONE
    -- edit vocabulary across the page: a panel row, a tag and a link are edited
    -- alike, and the derived cell — a coverage, a count, a link's type — never
    -- opens.
  , "    popupKeys(\"links\", () => lmount, {"
  , "      editing: lediting,"
  , "      editKeys: (k) => {"
  , "        if (k === \"TAB\" || k === \"S-TAB\") hop();"
  , "        else if (k === \"RET\") commitLink(edit.row);"
  , "        else return false;   // ESC is the keymap's, and puts the link back"
  , "        return true;"
  , "      },"
  , "      keys: (k) => {"
  , "        if (k === \"o\") {"
  , "          const link = pointedLink();"
  , "          const b = opening;"
  , "          shutLinks();"
  , "          if (link) openLink(b, link);"
  , "        }"
  , "        else if (k === \"RET\") openLinkEdit();"
  , "        else return false;"
  , "        return true;"
  , "      },"
  , "    });"
    -- MOVE, RENAME, FLAG, REMOVE, ADD — the same shape, one popup over.  `RET'
    -- opens the rename and, with the overlay up, commits it; `d'/`D'/`u' are the
    -- deletion gesture, spelled here as on the other three surfaces and guarded
    -- against a HELD key the same way; `+' raises the add field OVER this popup,
    -- the one raise `sole' exempts and the reason the guard above exists.
  , "    popupKeys(\"tags\", () => tmount, {"
  , "      editing: renaming,"
  , "      editKeys: (k) => {"
  , "        if (k !== \"RET\") return false;   // ESC is the keymap's, and puts the tag back"
  , "        renameTag(edit.row, el(\"tname\").value);"
  , "        return true;"
  , "      },"
  , "      keys: (k, e) => {"
  , "        if (k === \"RET\") openRename();"
  , "        else if (k === \"+\") addFlow();"
  , "        else if (!flagPress(k, e, TFLAGS)) return false;"
  , "        return true;"
  , "      },"
  , "    });"
  , ""
  , "    function apply(frame) {"
      -- A WRITE COMES BACK THROUGH THE WATCH, for the sheet as for the table. The
      -- command route never writes the store, so a `set-state' or a `set-title'
      -- made from the document leaves the sheet holding what the file said BEFORE
      -- it; the frame naming this row is when there is something fresher to read.
      -- Never while an edit is open, a re-read pulling the model out from under
      -- it.
  , "      const moved = frame.op === \"delete-row\" ? frame.id : (frame.row || {}).id;"
      -- NEVER OVER UNCOMMITTED WORK, AND NEVER UNDER THE READER'S HANDS.
      -- `reload' rebuilds both panes — `prows' is rebuilt, `baseProps' re-pinned
      -- and `drawProps' clears `#mprops.on' — so a re-read while the reader has
      -- an open panel row, a committed drawer edit they have not flushed, or
      -- merely the panel's CURSOR, throws that away silently and under a `synced'
      -- header; `pnav()' is in the guard because losing the keys back to the
      -- document pane mid-read is the same theft one grain smaller.  And the
      -- reader's own `t', `:' or `S-<up>' from inside the sheet is what CAUSES
      -- the event, so that is the ordinary case rather than a race.
  , "      if (editing && !raw && !sheetOpen() && !dirty() && !pnav()"
  , "          && moved === editing.id)"
  , "        reload();"
  , "      if (!table) return;"
  , "      // Under a filter the loaded rows are the server's answer to a query,"
  , "      // and only it knows whether the changed row still matches: ask again."
      -- The refetch is where the rows leave for a filtered client, so it is the
      -- refetch that has to land the archive's anchor — hence `settled' rather
      -- than the first-row landing every other caller of `fetchRows' takes.
  , "      if (query) return void (clearTimeout(requeryAt),"
  , "        requeryAt = setTimeout(() => fetchRows(settled), 250));"
  , "      if (frame.op === \"upsert-row\") table.upsertRow(frame.row);"
  , "      else if (frame.op === \"delete-row\") table.deleteRow(frame.id);"
      -- And the splice is where they leave for an unfiltered one.  The renderer
      -- has already kept the cursor by the time this runs — on its row while that
      -- row is there, else at the same visual place — so this only ever overrides
      -- that with the anchor, and only for the frame taking point's own row out.
  , "      else return;"
  , "      settled();"
  , "    }"
  , "    function listen() {"
  , "      const scheme = location.protocol === \"https:\" ? \"wss\" : \"ws\";"
  , "      // The rows came over HTTP; the socket's own set-rows would resend them."
  , "      socket = new WebSocket(`${scheme}://${location.host}/ws?bootstrap=off`);"
      -- The other half of the wash, and the only one a reader can sit in for
      -- minutes: a page whose socket is gone goes on showing rows nothing can
      -- correct.  Set rather than stepped — a connection refused closes without
      -- ever opening — and the delay keeps a reconnect that costs one
      -- revalidation from dimming anything.
  , "      socket.onopen = () => {"
  , "        backoff = 1000; wash.want(\"socket\", 0);"
  , "      };"
  , "      socket.onmessage = (e) => apply(JSON.parse(e.data));"
  , "      socket.onclose = (e) => {"
  , "        socket = null;"
  , "        wash.want(\"socket\", 1);"
  , "        // The columns moved, which SCHEMA.md's row ops cannot say: the"
  , "        // mount has to go.  Every other close — a backlog abandoned under"
  , "        // a write storm (`resync'), a restarted daemon, a dead network —"
  , "        // costs rows and nothing else, and the page stays where it was."
  , "        if (e && e.reason === \"view-changed\") remount(); else resync();"
  , "      };"
  , "    }"
  , "    // A lost socket costs rows and keeps the page.  Ask"
  , "    // /headlines for the applied query with the tag the last answer carried:"
  , "    // an unmoved store answers 304 and costs a header exchange, a moved one"
  , "    // answers with rows that drop into the table standing here.  The mount"
  , "    // stays through both — the sheet, the palette, the selection and the URL"
  , "    // with it — which is what makes an editor's write storm a row refresh"
  , "    // rather than the page reloading under a reader's hands."
  , "    function resync() {"
  , "      if (!table) { start(); return; }   // nothing mounted yet: this is a boot"
  , "      const asked = query;"
  , "      load(asking(asked), etag).then((a) => {"
  , "        // The close reason is not trusted for this: a daemon restarted while"
  , "        // this page was away had no socket to send `view-changed' down, and"
  , "        // its columns can still have moved."
  , "        if (a.view && !sameColumns(a.view.columns || [])) { remount(); return; }"
      -- A repaint of the SAME view is a third road the archive's rows can leave
      -- by: the write landed while the socket was down and the reconnect's
      -- answer is the first the page has seen without them.
  , "        if (a.view && query === asked) { paint(a); settled(); }"
  , "        backoff = 1000;"
  , "        listen();"
  , "        append(\"ws\", \"info\", a.view ? \"reconnected · rows refreshed\" : \"reconnected\");"
  , "      }).catch((e) => {"
  , "        if (e.indexing) return indexing(e.indexing);"
  , "        // A newer query is already fetching and will paint what it gets;"
  , "        // the socket is all this call still owed."
  , "        if (e.name === \"AbortError\") { listen(); return; }"
  , "        quiet(e); again();"
  , "      });"
  , "    }"
  , "    // The columns are the one part of a view rows cannot carry, so they are"
  , "    // compared whole: the state column's badge palette rides inside them,"
  , "    // and a key-by-key check would let it move unnoticed."
  , "    const sameColumns = (next) => JSON.stringify(next) === JSON.stringify(cols);"
  , "    function again() {"
  , "      append(\"ws\", \"warn\", `disconnected · retrying in ${Math.round(backoff / 1000)}s`);"
  , "      setTimeout(resync, backoff);"
  , "      backoff = Math.min(backoff * 2, 30000);"
  , "    }"
  , "    // The server binds before it walks the tree, so the first fetch of a"
  , "    // cold daemon is a 503: show what it is doing and ask again in a second."
  , "    // A daemon that restarts under a live page lands here too, and comes"
  , "    // back through `resync' — the page it left is still on screen."
  , "    function indexing(b) {"
  , "      append(\"boot\", \"info\","
      <> " `indexing … ${b.elapsed}s · the table opens when the walk lands`);"
  , "      setTimeout(resync, 1000);"
  , "    }"
    -- AFTER is what a canned view wants doing once its own rows are up, given the
    -- server's match count.  An argument rather than a variable this arms and
    -- disarms, so it belongs to the boot it was passed to and a boot that never
    -- lands cannot leave one behind for the next.  It also carries the LANDING,
    -- which is why a caller that passes one lands nothing here: a pop puts the
    -- cursor back on the row its drill was launched from, and this door must not
    -- land row one over it first.
  , "    function start(after) {"
  , "      // A `?q=' in the address bar is a filtered view, and so is a bare"
  , "      // boot: the boot asks for whichever it is and `mount' opens the"
  , "      // filter showing it.  Every return through this door — a reload,"
  , "      // `view-changed', `g' — restores it the same way, since they all"
  , "      // re-fetch and re-mount; a reconnect never comes here at all."
  , "      // The default is written into the URL where it was injected, so what"
  , "      // the page shows and what the address bar says are the same query"
  , "      // from the first paint on."
  , "      const asked = (query = bootQuery());"
  , "      if (!params().has(\"q\")) remember(asked);"
      -- SWAP ON THE ANSWER.  A boot has nothing on screen, so it takes the first
      -- page it can get and pulls the rest in behind the painted table.  A
      -- RE-APPLICATION has a whole table standing — `g', `a', `@', a pop, a
      -- `view-changed' remount — and asks for the WHOLE answer once, a page-sized
      -- mount here replacing a complete table with a partial one and reflowing
      -- the pager and the hint under the reader a moment later.  Under either,
      -- the rows that are up STAND until the new ones are in hand and the swap is
      -- one mount; the wash is what says they are on their way.
  , "      const swap = !!table;"
  , "      const narrow = asking(asked) + (asked ? \"&\" : \"?\");"
  , "      viewing(load(swap ? asking(asked) : `${narrow}limit=${PAGE}`)).then((a) => {"
  , "        mount(a.view);"
      -- A MOUNT LANDS, and a BOOT IS AN APPLIED VIEW.  A new mount has no cursor
      -- of its own — the renderer selects nothing until something asks it to — so
      -- a page that landed nothing here would open with `d', `D' and `RET' all
      -- answering `no row' until the reader pressed `n'.  It is the apply landing
      -- and goes through `land' like every other, so row one is spelled in
      -- exactly one place.  A caller with an opinion — a pop, through `applyView'
      -- — lands inside AFTER instead and this one stands aside for it.
  , "        if (after) after(a.total); else land(null);"
  , "        listen();"
      -- The full set arriving behind the first page LANDS NOTHING: the cursor
      -- this just put on row one is the reader's from the first paint on, and
      -- `paint' keeps it the way the renderer keeps every selection — on its row
      -- while the row is there.  One landing per mount, at the mount.
  , "        // The rest behind the painted table: n/p, sort and materialize all"
  , "        // want the whole answer, and the renderer holds it without the DOM."
  , "        if (!swap && a.total > (a.view.rows || []).length)"
  , "          load(asking(asked))"
  , "            .then((b) => { if (table && query === asked) paint(b); arm(a.total); })"
  , "            .catch(quiet);"
  , "        else arm(a.total);"
  , "      }).catch((e) => {"
  , "        if (e.indexing) return indexing(e.indexing);"
  , "        quiet(e); if (e.name !== \"AbortError\") again();"
  , "      });"
  , "    }"
    -- The first line of the log, and an ordinary one: the strip is never
    -- cleared, so the boot stays in the scrollback under everything that
    -- follows it rather than being a placeholder something has to take away.
  , "    append(\"boot\", \"info\", \"loading …\");"
  , "    start();"
  , "  </script>"
  ]

-- | 'captureCodes' as the objects the page's own completion reads: the code and
-- the one line saying what it does, in the order the list declares them.  The
-- same shape @GET \/capture@ serves, so a client that reads it there and this
-- page's spliced copy cannot come to describe two different subsets.
codeList :: [Value]
codeList = [ object ["code" .= code, "means" .= means] | (code, means) <- captureCodes ]
