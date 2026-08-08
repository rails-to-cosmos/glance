// Boots the shell's inline glue under node and reports what it asked the
// server for and what survived what happened to it next.  The glue is the
// page's own, extracted from a rendered `/' by TestServe; the browser around it
// is stubbed down to what a boot touches, so what this measures is the page's
// behaviour — which string-matching the glue cannot answer, since a call that
// is present and never reached matches just the same.
//
//   node shell-harness.js DIR SEARCH TOTAL [KEYS] [ACTS]
//
// DIR holds `shell.js' (the glue), `keys.json' (the page's keymap blob) and
// `cfg.json' (the configuration blob the glue reads as CFG).
// SEARCH is `location.search' the page opens on and TOTAL what the server
// reports as `X-Glance-Total', which is what decides whether the boot pulls
// the rest of the set in behind the first page.  KEYS is an optional
// space-separated list of `KeyboardEvent.key' names pressed over the table once
// the boot has settled — a `%CODE' tail naming the physical key under the
// character, the way a non-Latin layout delivers one (`т%KeyN').  Both overlays
// are opened through the page's own keys:
// `Enter' materializes the first row and `/' raises the filter palette.  ACTS
// is what happens after that, one verb at a time, each settled before the next:
//
//   close:REASON  the socket closes, the way the server closes one
//   sheet:TEXT    TEXT typed into the open sheet's textarea
//   dtin:TEXT     TEXT typed into the document's title overlay, one field
//                 over the headline's title; `RET' has to have opened it over
//                 the headline at point first
//   dpara:TEXT    TEXT typed into the paragraph overlay, which is a textarea
//                 over the block at point; `RET' opens one.  `_' is a space,
//                 `|' a newline and `~' a literal bar — an org table row
//                 spells itself `~a~b~'

//   pkey:I=TEXT   TEXT typed into property row I's key field, the row having
//                 been opened for editing first — a closed one has no fields
//   pval:I=TEXT   TEXT typed into property row I's value field, likewise
//   filter:TEXT   TEXT typed into the raised palette
//   frame:OP=IDS  row frames delivered down the LIVE SOCKET, the way the
//                 watcher delivers them — `frame:upsert=r1' re-sends a row that
//                 moved, `frame:delete=r1,r2' says two are gone and the served
//                 set loses them
//   unserved:IDS  the applied query stops matching IDS: /headlines answers
//                 without them and the tag steps, the rows staying the store's
//   moved         the store moves: a new ETag, and a row more to fetch
//   recolumn      the store moves and its columns move with it
//   rewritten     the file behind the open sheet moves: a new digest
//   press:KEY     KEY pressed, so a key can follow an act rather than precede
//                 it; `C-x' and `S-Tab' spell the modifiers and `т%KeyN' the
//                 physical key under a character
//   stuck:KEY     KEY down with no release and `repeat' unset — the native
//                 window's lying auto-repeat; two in a row are one held key
//   click:I       row I of the modal mount that is up clicked, which is the one
//                 way a cursor moves out from under an open edit overlay
//   theme:NAME    NAME picked in the settings sheet's theme select, event and all
//   pinclick      the chip strip's pin button clicked, which is the touch door
//                 to whatever the consumer wired into `onPin'
//   type:TEXT     TEXT typed into the value palette's field, which narrows it —
//                 `/' has to have put the palette in that mode first
//   tname:TEXT    TEXT typed into the tags popup's rename overlay, which `RET'
//                 has to have opened over the tag at point first
//   ktag:TEXT     TEXT typed into the capture form's tag field, `+' having
//   ktext:TEXT    raised the form; `ktext' is its line, and `kf:TEXT' types
//   kf:TEXT       into whichever grown template field holds the focus
//   ltitle:TEXT   TEXT typed into the link popup's edit overlay, whose two
//   lurl:TEXT     fields are what the entry calls the link and where it points;
//                 `RET' has to have opened it over the link at point first
//   assign:A,B,C  the which-key assignment run over that cycle, as the pure
//                 function it is
//   cells:K@C     the edit overlay's cell resolution run over the keys K
//                 against the column keys C, likewise as the pure function
//   priorities:P  per-row priority cells, positional and comma-separated:
//                 `priorities:A,,C' is `[#A]', none, `[#C]' — which is the mixed
//                 set org's per-entry cycle is about
//   refuse        the next /command refuses — every row it named, or the
//                 capture whole, which names none
//   bare          the mounted handle loses its mark calls, the way an older
//                 table-view.js never had them
//   pageless      and its pager calls, the way one older still never had those
//   sortless      and its programmatic sort, which `^' and the agenda ask for
//   crumbless     and its crumb trail, which `@' needs before it will drill
//   onemailto     the row points at one link that is not http(s)
//   partly        two of the three rows carry `web' and the third does not,
//                 which is the mixed set manage-tags normalizes up
//   untagged      no row carries a tag at all
//   unknownrows   the store knows none of the rows the palette named
//   onelink       the row `o' names points at exactly one place
//   nolinks       and at none at all — three is the default, which is the popup
//   everytype     one link of every type the server derives, so the popup's
//                 badge column and the commit's judgement span the vocabulary
//   noreferences  nothing points at the row `@' names
//   rows:N        the store holds N rows rather than the three at the top
//   paged:N       the renderer shows N of them a page, so there are pages to
//                 turn and ends of a page to reach
//   spam:N        N distinct lines appended to the page's event log, which is
//                 the only way to reach a ring that holds five hundred
//   offline       the daemon goes away: every request after this fails
//   online        and comes back, which is what the retry finds
//   hang          /headlines stops answering, so a swap can be watched in flight
//   deliver       and answers everything held since
//   wait:MS       MS milliseconds pass, which is what a delayed state needs
//
// The answer is what the page asked for and what it still holds afterwards.
const fs = require("fs");
// STORE is what the browser already REMEMBERS, `KEY=VALUE' pairs joined by
// commas.  It is argv rather than an act because a preference the BOOT reads is
// unreachable from an act: every act runs after the glue has been eval'd.
const [dir, search, total, keys, acts, store] = process.argv.slice(2);

// Every /headlines URL the page asked for, in order, and the tags it sent with
// them — a revalidation is what a cheap reconnect looks like from the server.
const asked = [];
const tags = [];
// The store this harness stands in for, and the tag that says which version of
// it callers hold.  `moved' and `recolumn' step it.
// Three, because a walk needs somewhere to walk to: `m' marks and steps, so a
// one-row store cannot tell marking from advancing.
// A TOTAL OF NONE IS AN EMPTY STORE, since the count the server reports is the
// count of the set it is answering with: a boot told there are no matches
// cannot also be handed rows.  Argv rather than an act, because an act cannot
// reach it in time — every one of them runs after the boot has painted.
let rows = +total === 0 ? [] : ["one", "two", "three"].map((title, i) =>
  ({ id: `r${i + 1}`, cells: { state: "TODO", title, tag: ":web:" } }));
// The rows the APPLIED QUERY no longer matches: still the store's — the socket
// carries a row op whatever the client asked for — and out of what /headlines
// answers, which is what an archived row looks like to a client filtered to the
// active ones.  `unserved' moves them here and a frame can still be built from
// one afterwards.
let hidden = [];
// The state column carries its badge palette, which is where the value palette
// C-c C-t raises reads its COLOURS — the keywords themselves are /keywords'
// answer, and a keyword no badge names simply carries no hue.
// ONE of the two declares `sortable', which is the renderer's opt-in: `^' has
// to reach a column that sorts and refuse one that does not, and a pair with
// one of each is what makes both answers reachable.  The real producer opts
// every column in.
let columns = [
  { key: "state", sortable: true,
    badges: [ { value: "TODO", color: "#e0af68", group: "active" }
            , { value: "READING", color: "#bb9af7", group: "active" }
            , { value: "DONE", color: "#73daca", group: "inactive" } ] },
  { key: "tag" },
];
// The view's own sort, which a mount takes its order from — the producer always
// declares one, over a column this stub has (the real one's is `scheduled').
// It is what the first `^' on that column REVERSES.
const declaredSort = { column: "state", ascending: true };
let tag = "\"t0\"";
// Set by `noreferences': nothing points at the row `@' names, so the ref query
// answers empty — which is what the drill's probe reads before it applies
// anything.  Targeted at the ref query alone, so the boot and the parity
// baseline still answer for the whole store.
let unreferenced = false;
let served = +total;
// The subtree behind /headline, in the two shapes the route serves it in — the
// raw text, and the body with the three regions lifted out — plus the cells of
// the headline itself, the entries hanging under it, and the digest a write is
// pinned to.  The split is the server's, so what the sheet gets here is what a
// real one would hand it.
//
// `ORG_GLANCE_ID' is in the org text and NOT in the properties: it is a hidden
// key the server keeps for itself, so the sheet never sees it and never sends
// it back.  The planning line and the logbook are the other two regions.
//
// IT HAS A CHILD, which is what the sub-addressing walks into: `?child=0' is the
// entry `** two', and the row's own body stops where that child's stars begin —
// `ownLines', so the same bytes are never drawn twice, once as a paragraph and
// once as the child that owns them.
// THE GRAINY BODY, which `grain' swaps in: a lead-in paragraph, a three-item
// list with one item carrying a continuation line and a nested sub-item, a
// `#+begin_quote' block of two paragraphs, and a closing paragraph.  Every stop
// kind the walk has is in it, and in the order the walk meets them.
//
// The list's second item is separated by a BLANK LINE, which org lets stand
// inside a list and 1173 item pairs of the corpus rely on — so the run is one
// list rather than two.
const grainBody = [ "* TODO one",
                    "lead in",
                    "- alpha",
                    "  more alpha",
                    "  - nested",
                    "",
                    "- beta",
                    "- gamma",
                    "",
                    "#+begin_quote",
                    "quoted one",
                    "",
                    "quoted two",
                    "#+end_quote",
                    "",
                    "tail para",
                    "** two",
                    "child body", "" ].join("\n");
// THE CHECKY BODY, which `checky' swaps in: a four-item list wearing org's
// three checkbox states and one bare item — what `SPC' and `C-c C-c' toggle,
// and the stop that refuses.
const checkyBody = [ "* TODO one",
                     "- [ ] alpha",
                     "- [X] beta",
                     "- [-] gamma",
                     "- delta",
                     "** two",
                     "child body", "" ].join("\n");
// THE TABLED BODY, which `tabled' swaps in: a lead-in paragraph, a FOUR-LINE
// org table with a `|---+---|' rule among its rows, a two-item list and a
// closing paragraph.  MIXED on purpose — the count of stops end to end is what
// says the table takes its place in the one walk rather than a walk of its own.
//
// A LINE IS A LEAF, rule included: the table is one coarse stop and then its
// four rows, which is the list's shape with the item rule at a line.
const tabledBody = [ "* TODO one",
                     "lead in",
                     "| a | b |",
                     "|---+---|",
                     "| 1 | 2 |",
                     "| 3 | 4 |",
                     "",
                     "- alpha",
                     "- beta",
                     "",
                     "tail para",
                     "** two",
                     "child body", "" ].join("\n");
// THE LINKY BODY, which `linky' swaps in beside its `linkylinks': every shape
// the display rule has to tell apart, in the two places that draw one.
//
//   line 0  the headline, whose TITLE holds `[[T][D]]'
//   line 1  a paragraph with `[[T][D]]' and `[[T]]' in it
//   line 3  a paragraph with a BARE url written TWICE — where the answer holds
//           ONE entry, `/links' keeping the FIRST spelling of a target and no
//           other (`Glance.Query.orgLinks'), so the second occurrence has no
//           span and is drawn as the text it is
//
// The spans below are the ones a scan of this text measures, worked out once
// and written down; `desc' is what `Glance.Query.linkShown' answers for each,
// since the DISPLAY rule is the server's and the page only draws what it is
// handed.
const linkyBody = [ "* TODO one [[https://t.example/][the title link]]",
                    "see [[https://a.example/][alpha]] and [[https://b.example/]] here",
                    "",
                    "bare https://c.example/ then https://c.example/ twice",
                    "** two",
                    "child body", "" ].join("\n");
const linkyTitle = "one [[https://t.example/][the title link]]";
const linkyLinks = [
  { target: "https://t.example/", desc: "the title link", type: "https", span: [11, 49] },
  { target: "https://a.example/", desc: "alpha", type: "https", span: [54, 83] },
  { target: "https://b.example/", desc: "https://b.example/", type: "https", span: [88, 110] },
  { target: "https://c.example/", desc: "https://c.example/", type: "https", span: [122, 140] },
];
let linky = false;
let grainy = false;
let tabled = false;
let checky = false;
const org = "* TODO one\nSCHEDULED: <2026-08-01 Sat>\n:PROPERTIES:\n"
  + ":ORG_GLANCE_ID: r1\n:EFFORT: 0:30\n:END:\n:LOGBOOK:\n- moved here\n:END:\n"
  + "first para\n\nsecond para\n** two\nchild body\n";
const body = "* TODO one\nfirst para\n\nsecond para\n** two\nchild body\n";
const properties = [["EFFORT", "0:30"]];
const planning = [["SCHEDULED", "<2026-08-01 Sat>"]];
const logbook = ":LOGBOOK:\n- moved here\n:END:\n";
let digest = "d0";
/** The priority the MATERIALIZED entry carries, which `priorities' moves with
 * the store's own cell — the sheet reads its cells off the answer rather than
 * off a table row, so a case about the document owes both. */
let headPriority = null;
/** What GET /headline answers with, for the ROW and for the one entry under it.
 * The navigation fields are the whole of the sub-addressing contract: `child' is
 * the index this answer is FOR, `parent' the one DEL climbs to (null being the
 * row) and `children' the entries hanging under it with the index each answers
 * to. */
const subtree = (child) => (child === null
  ? { id: "r1", file: "a.org", child: null, parent: null, path: ["one"],
      cells: { state: "TODO", priority: headPriority,
               title: linky ? linkyTitle : "one", tags: "" },
      children: [ { index: 0, level: 2, state: null, priority: null,
                    title: "two", tags: ":web:" } ],
      level: 1, properties, planning, logbook, digest,
      // Where the TITLE CELL starts in the file, which is what lets the page
      // tell which of the row's links are inside that cell.  The server has the
      // sub-span; this stands in for it.
      titleAt: linky ? 7 : 11,
      // THE SUBTREE'S OWN EXTENT, which is what turns an element's LINE range
      // into the FILE range `o' filters links by.  The grainy body is served as
      // its own org text, so the two differ by nothing and the arithmetic is
      // readable in the case.
      // The row's link scan rides the materialize, the server's own shape:
      // one answer, so the display is compact from the first frame and the
      // suite sees links with no second fetch to wait on.  The list is the
      // same `links' variable the `/links' stub serves — one source, like the
      // server's one scanner — configured by `linky'/`grainlinks'; the plain
      // fixtures carry none, their canned spans describing the table popup's
      // own text.
      links: linky || grainy ? links : [],
      org: linky ? linkyBody : grainy ? grainBody : tabled ? tabledBody
           : checky ? checkyBody : org,
      span: { start: 0,
              end: (linky ? linkyBody : grainy ? grainBody : tabled ? tabledBody
                          : checky ? checkyBody : org).length },
      body: linky ? linkyBody : grainy ? grainBody : tabled ? tabledBody
            : checky ? checkyBody : body,
      ownLines: grainy ? 16 : tabled ? 11 : checky ? 5 : 4 }
  : { id: "r1", file: "a.org", child: 0, parent: null, path: ["one", "two"],
      cells: { state: null, priority: null, title: "two", tags: ":web:" },
      children: [],
      org: "** two :web:\nchild body\n",
      body: "** two :web:\nchild body\n", ownLines: 3, level: 2,
      properties: [], planning: [], logbook: "", digest,
      links: linky || grainy ? links : [] });
/** Every subtree a POST was aimed at, as `id' or `id#child' — which is the whole
 * of what says WHICH extent a commit named. */
const wroteAt = [];
/** And every subtree a GET asked for, the same way: the sheet re-reads itself
 * after a commit and whenever a socket frame names the row it is standing on, so
 * what says a re-read HAPPENED is the request rather than the answer — the
 * canned one never moves. */
const readAt = [];
// Every POST /headline body, which is the whole of what a sync can be observed
// to have written: the rows come back over a socket this harness does not run.
const writes = [];
// Every structured command the page posted, as the body it sent — which is the
// whole of what a key like `D' can be observed to have done, the rows coming
// back over a socket this harness does not run.
const commands = [];
let refusing = false;
// The keyword layers behind /config, and every write to one.  The system layer
// carries no digest: it is a file that does not exist yet, which is the shape
// the settings sheet has to be able to create.
//
// SERVED OUT OF ALPHABET, on purpose: the server's order is the walk's — where
// the directories turned up — and the sheet's is system first and then the tags
// in their own alphabet, so a fixture already in order could not tell the two
// apart.
// `keywords' is the same lines PARSED, which the server serves beside them so
// the states table reads structure where the keywords box reads text.  Canned
// here like the lines, and kept in step with them by hand — the point of a
// fixture is to be an independent statement of what an answer looks like.
let layers = [
  { path: "/o/.org-glance/config/system.org", tag: null,
    lines: ["#+TODO: TODO | DONE"],
    keywords: { active: ["TODO"], inactive: ["DONE"] },
    template: "", digest: "" },
  { path: "/o/.org-glance/config/tags/film.org", tag: "film",
    lines: ["#+TODO: WATCHING | WATCHED"],
    keywords: { active: ["WATCHING"], inactive: ["WATCHED"] },
    template: "", digest: "f1" },
  { path: "/o/.org-glance/config/tags/book.org", tag: "book",
    lines: ["#+TODO: TODO READING | READ"],
    keywords: { active: ["TODO", "READING"], inactive: ["READ"] },
    template: "* %?", digest: "c1" },
];
const configWrites = [];
let configTick = 1;
// What /keywords resolves for the rows a command names: the classification
// chain, widest source first, each source holding what it is the widest to
// declare.  Canned, the way the layers above are: the resolution is the
// server's and TestConfig is where the rule itself is tested — what the page
// owes is drawing whatever comes back, in the order it comes back.
let sources = [
  { source: "default", active: ["TODO"],    inactive: ["DONE"] },
  { source: "book",    active: ["READING"], inactive: ["READ"] },
  { source: "file",    active: ["LATER"],   inactive: [] },
];
// Every /keywords URL the page asked for, which is the whole of what says WHICH
// rows it resolved the palette for.  `stalling' holds one out forever.
const resolved = [];
let stalling = false;
// What /links answers for the row `o' names.  Canned like the layers and the
// resolution above: the extraction is the server's and TestQuery is where the
// rule is tested — what the page owes is the gesture over whatever comes back,
// which is why `onelink' and `nolinks' are acts.
// The TYPE is the server's own word for the target (`Glance.Query.linkType'):
// canned here like the rest of the answer, since the derivation is TestQuery's
// and what the page owes is the badge cell and the commit it decides.
// The SPAN is the half-open char range the link occupies in its file, which is
// what makes the answer writeable: `RET' sends it back as the range to splice.
// Canned like the rest — the offsets are the scanner's and TestQuery is where
// they are measured — so what the page owes is handing back the range it was
// given, under the digest it came with.
let links = [
  { target: "https://one.example/a", desc: "First reference", type: "https",
    span: [10, 48] },
  { target: "https://two.example/b", desc: "Second reference", type: "https",
    span: [60, 99] },
  { target: "mailto:t@example.org", desc: "mailto:t@example.org", type: "mailto",
    span: [120, 140] },
];
// And the digest that answer carried, which an edit pins: the spans describe
// the file as the store read it, so a file that has moved refuses.
let linkDigest = "d0";
// Every /links URL asked for, and every tab the page opened.
const linked = [];
const opened = [];
// What /tags answers for the rows a tag command names: each row's own tags,
// folded the way the store reports them, plus the tree's whole vocabulary,
// which is what `/' over the palette narrows.  Canned like the resolution
// above — the reading is the server's and TestQuery is where the rule is
// tested; what the page owes is the union, the partial counts and the toggle
// over whatever comes back.
//
// A tag command does NOT move this, deliberately: the route never writes the
// store, so a palette that re-read after a commit would answer with what the
// files said before it.  What the list shows next has to come out of the
// command's own per-id answer.
let rowTags = { r1: ["web"], r2: ["web"], r3: ["web"] };
let vocabulary = ["archive", "book", "web", "work"];
// And how many ROWS the whole tree has under each of them, which is the popup's
// third column and the one number no arithmetic over the rows in hand recovers.
// Canned like the rest: the count is the store's and TestServe's route cases are
// where it is measured.
const tagCounts = { archive: 12, book: 3, web: 40, work: 7 };
// Every /tags URL asked for, which is what says WHICH rows the palette
// resolved for.
const tagged = [];
// The SAVED VIEWS `system.org' names, which `g' and `a' apply and the settings
// sheet edits beside that layer's cycle.
let viewQuery = "state:*active*";
let agendaQuery = "state:*active* -planned:*empty* sort:scheduled";
// And the tree's per-theme state hues, the flat list the answer serves.
let stateHues = [];
// And the capture target it names, which is the other line of that file the
// sheet edits — plus the path the server resolves it to, which is what a
// capture reports back and the log names.
let captureLine = "";
const captureTarget = "/o/inbox.org";
// The row a capture makes, which is what point lands on when the watch delivers
// it: a minted `ORG_GLANCE_ID' for a blob, the target file's ordinal for a line
// in the inbox.
const capturedId = "r3";
// What `/capture' answers.  The codes are the expansion subset the settings box
// completes over, and the prompts are what a tag's own template asks — `book'
// has one, every other tag has none, so the chain can be walked with a step in
// the middle and without one.
const captureCodes = [
  { code: "%?", means: "where the text you type lands" },
  { code: "%U", means: "the moment of capture, inactive" },
];
// A tag whose layer configures a template ASKS nothing unless its template
// spells a `%^{PROMPT}', so `template' and a non-empty `prompts' are two facts
// — the server answers them apart and this stub does too.  `film' is the tag
// with a template and no ask.
const capturePrompts = { book: ["Author"] };
const captureTemplates = ["book", "film"];
// Every /capture URL asked for, which is what says whether the chain resolved
// the tag before it asked the reader anything.
const captureAsked = [];

globalThis.location = { search, protocol: "http:", host: "h", pathname: "/" };
globalThis.history = {
  // The page writes its applied query here; the search string it leaves behind
  // is the link a reload would come back to.
  replaceState: (_state, _title, url) => {
    location.search = String(url).startsWith("?") ? url : "";
  },
};
const answer = (status, body, headers) => Promise.resolve({
  ok: status >= 200 && status < 300,
  status,
  headers: { get: (name) => (headers || {})[String(name).toLowerCase()] || null },
  json: () => Promise.resolve(body),
  text: () => Promise.resolve(""),
});
// Set by `offline' and taken back by `online': the daemon is gone and every
// request fails at the network.
let down = false;
// Set by `hang': /headlines answers nothing until `deliver' lets it go, which
// is the only way to observe the page WHILE a swap is in flight — everything
// else here settles as a microtask and one turn of the loop is past it.
let hanging = false;
const held = [];
// The same pair over `POST /config', which is what lets a script type into the
// settings sheet while its own write is out.
let changing = false;
const cheld = [];
/**
 * The rows a URL asks for.  The server caps a `limit=' fetch, so a page-sized
 * first paint really is one page: a swap that asks for the whole set can be
 * told from a boot that asks for a page by what each of them gets back.
 */
const capped = (url, list) => {
  const at = /[?&]limit=(\d+)/.exec(String(url));
  return at ? list.slice(0, Number(at[1])) : list;
};
globalThis.fetch = (url, init) => {
  if (down) return Promise.reject(new Error("fetch failed"));
  const sent = ((init || {}).headers || {})["if-none-match"];
  if (String(url).startsWith("/headlines")) {
    asked.push(url);
    if (sent) tags.push(sent);
    const send = () => {
      // The server's own answer to a tag it still stands behind: no body at all.
      if (sent === tag) return answer(304, null, {});
      const empty = unreferenced && String(url).indexOf("q=ref%3A") !== -1;
      return answer(200, { title: "t", columns, sort: declaredSort,
                           rows: empty ? [] : capped(url, rows) },
                    { "x-glance-total": empty ? "0" : String(served), etag: tag });
    };
    if (hanging) return new Promise((go) => held.push(() => go(send())));
    return send();
  }
  if (String(url) === "/command") {
    const sent = JSON.parse((init || {}).body || "{}");
    commands.push(sent);
    // Capture names no row, so it answers in its own shape: the file the
    // server picked and the digest that file carries now.
    if (sent.name === "capture")
      return refusing
        ? answer(400, { error: "#+GLANCE_CAPTURE_TARGET: /x.org is an absolute path" })
        : answer(200, { ok: true, file: captureTarget, digest: "d1", id: capturedId });
    return answer(200, {
      results: (sent.ids || []).map((id) =>
        refusing ? { id, ok: false, error: "a.org changed on disk" }
                 : { id, ok: true, digest: "d1" }),
    });
  }
  // Not gated on `refusing': what that flag stands for is a WRITE the server
  // turns down, and a chain that could not resolve its tag would never reach one.
  if (String(url) === "/capture" || String(url).startsWith("/capture?")) {
    captureAsked.push(url);
    const at = /[?&]tag=([^&]*)/.exec(String(url));
    const tag = at ? decodeURIComponent(at[1]) : null;
    return answer(200, {
      template: !!(tag && captureTemplates.indexOf(tag) !== -1),
      prompts: tag ? (capturePrompts[tag] || []) : [],
      tags: vocabulary,
      codes: captureCodes,
    });
  }
  if (String(url).startsWith("/keywords?ids=")) {
    resolved.push(url);
    // Never settling, which is the only way to observe the moment the overlay
    // is up and the resolution is not: everything else here answers as a
    // microtask, and one turn of the loop is past it.
    if (stalling) return new Promise(() => {});
    return refusing ? answer(400, { error: "GET /keywords?ids=<row id>" })
                    : answer(200, { sources, unknown: [] });
  }
  if (String(url).startsWith("/tags?ids=")) {
    tagged.push(url);
    if (stalling) return new Promise(() => {});
    if (refusing) return answer(400, { error: "GET /tags?ids=<row id>" });
    const ids = String(url).slice("/tags?".length).split("&")
      .map((p) => decodeURIComponent(p.slice("ids=".length)));
    return answer(200, {
      rows: ids.filter((id) => rowTags[id]).map((id) => ({ id, tags: rowTags[id].slice() })),
      vocabulary,
      counts: tagCounts,
      unknown: ids.filter((id) => !rowTags[id]),
    });
  }
  if (String(url).startsWith("/links?id=")) {
    linked.push(url);
    return refusing ? answer(404, { error: "no headline with id r1" })
                    : answer(200, { links, digest: linkDigest });
  }
  if (String(url) === "/config") {
    if ((init || {}).method !== "POST")
      return answer(200, { layers,
                           views: [ { id: "default", query: viewQuery }
                                  , { id: "agenda", query: agendaQuery } ],
                           themes: ["light", "dark"], colors: stateHues,
                           capture: captureLine,
                           keywords: { active: ["TODO"], inactive: ["DONE"] } });
    const sent = JSON.parse((init || {}).body || "{}");
    configWrites.push(sent);
    // The digest is the whole of the lock, an absent file's empty one included,
    // so a layer whose digest has moved refuses exactly as the server's does.
    const layer = layers.find((l) => l.path === sent.path);
    if (!layer || layer.digest !== sent.digest)
      return answer(409, { reason: "drift", digest: (layer || {}).digest || "",
                           error: "the config file changed on disk since it was read" });
    layer.lines = (sent.lines || []).filter(Boolean);
    // The server re-parses what it wrote, so the fixture does too: one line,
    // actives before the bar and done-like after.
    if (sent.lines !== undefined) {
      const body = (layer.lines[0] || "").replace(/^#\+TODO:/, "");
      const [act, done] = body.split("|");
      const words = (t) => String(t || "").split(/\s+/).filter(Boolean);
      layer.keywords = { active: words(act), inactive: words(done) };
    }
    // The saved views and the capture target are lines of the same file, so
    // they ride in one write under one digest — never a second request, which
    // a second digest would refuse anyway.  Each view is named on its own, so
    // one moved leaves the others where they are.
    const views = sent.views || {};
    if (views.default !== undefined) viewQuery = views.default;
    if (views.agenda !== undefined) agendaQuery = views.agenda;
    if (sent.colors !== undefined) stateHues = sent.colors;
    if (sent.capture !== undefined) captureLine = sent.capture;
    layer.digest = `c${(configTick += 1)}`;
    // Held by `chang', the settings sheet's half of `hang': `C-x C-s' syncs
    // mid-edit, so the state a script has to be able to sit inside is a write in
    // flight under a reader who is still typing.
    const send = () => answer(200, { path: sent.path, digest: layer.digest });
    if (changing) return new Promise((go) => cheld.push(() => go(send())));
    return send();
  }
  if (String(url).startsWith("/headline?")) {
    const named = /[?&]child=(\d+)/.exec(String(url));
    const child = named ? Number(named[1]) : null;
    if ((init || {}).method === "POST") {
      writes.push(JSON.parse((init || {}).body || "{}"));
      wroteAt.push(child === null ? "r1" : `r1#${child}`);
      // THE 200 CARRIES THE POST-WRITE DIGEST AND THE STORE LAGS IT: the real
      // server re-digests the file it just wrote, while `GET /headline' goes
      // on serving the store's PRE-write copy until the watch catches up.
      // This stub models the lag at its worst — the GET never catches up — so
      // a reload that trusts it reverts the pane and poisons the pin, which
      // is the regression the stale-drop cases hold shut.
      return refusing
        ? answer(409, { reason: "drift", digest,
                        error: "a.org changed on disk since this subtree was materialized" })
        : answer(200, { digest: `w${writes.length}` });
    }
    if (child !== null && child !== 0)
      return answer(404, { error: `r1 has no child ${child}; it holds 1` });
    readAt.push(child === null ? "r1" : `r1#${child}`);
    return answer(200, subtree(child));
  }
  return answer(404, {});
};
// The live socket, kept so a close can be delivered to it the way the server
// delivers one — with the reason that says which close it is.
let socket = null;
globalThis.WebSocket = function () {
  socket = this;
  this.close = () => { socket = null; };
  // A socket opens on a later turn than the one it was constructed in — the
  // page assigns `onopen' after the constructor returns — and the wash's other
  // half is cleared by that event, so a stub that never fired one would show a
  // reconnect that never finished.
  setTimeout(() => { if (socket === this && this.onopen) this.onopen(); }, 0);
};
// FOUR MOUNTS.  The page builds the table in `#app', the sheet's property panel
// in `#mptable', the link popup in its own overlay and the tags popup in its
// own, so everything a renderer holds PER MOUNT is held per instance here rather
// than once for the page: the cursor and its column, the page, the marks, the
// flags, the applied query and the crumb trail.  A remount replaces the table's
// instance and leaves the other three standing, which is what the shell relies
// on when it puts a sheet back up.
//
// The sheet's OTHER pane is not one of them: the structured document is the
// page's own widget, drawn into `#dlist', and it is read here off what it DREW.
//
// They differ in ONE thing, and it is the rows.  The table's are the STORE's —
// its `setRows' is a count and the rows it shows are `rows' above, which is what
// lets an act move the store and the table follow.  The two popups' are the
// shell's own models and arrive through `setRows', so those instances keep what
// they are handed.
let mounts = 0, sets = 0, raises = 0, pmounts = 0, psets = 0, lmounts = 0;
let tmounts = 0, tsets = 0;
// Every row count the shell has ever handed the TABLE, in order: one entry per
// mount and one per `setRows'.  A view swapping on its answer is one entry and
// a view painted before its answer is two, so what a reader would have seen
// flash is what this reads out.
const paints = [];
// And every row op the shell SPLICED into the table rather than refetching for,
// as `OP ID' in order — the unfiltered half of what a socket frame costs.  A
// shell that landed on the right row without ever splicing reads the same off
// the rows alone, so the calls are recorded as well as their effect.
const spliced = [];
// The last programmatic sort asked of a handle, which is the whole of what the
// agenda's own ordering can be observed to have done — and HOW MANY have been
// asked for, which is what says a sort was left alone: the renderer keeps its
// order across a `setRows', so a refetch that re-asserted one would show up
// here as a second call.
let sorted = null, sortCalls = 0, sortChain = [];
/** Q's tokens, on the separators the grammar names. */
const tokensOf = (q) => String(q || "").split(/[\s&]+/).filter(Boolean);
/**
 * The chain Q names, [] when it names none — the renderer's `sortsIn' over the
 * one token shape the shell can produce.  A `sort:' token names one column and
 * an optional direction, written order is precedence, and everything else in
 * the query is somebody else's business.
 */
const sortTokensIn = (q) => tokensOf(q)
  .filter((t) => t.startsWith("sort:") && t.length > "sort:".length)
  .map((t) => t.slice("sort:".length).split(":"))
  .map(([column, dir]) => ({ column, ascending: dir !== "desc" }));
/** Q with CHAIN's tokens in place of whatever sort tokens it carried. */
const withSort = (q, chain) => tokensOf(q)
  .filter((t) => !t.startsWith("sort:"))
  .concat(chain.map((k) => `sort:${k.column}${k.ascending ? "" : ":desc"}`))
  .join(" ");
/** The live table instance, the live panel instance and the two live popups.
 * The table starts as a standing empty one so a boot that never got to mount —
 * the indexing poll, an offline daemon — still answers about a table rather
 * than throwing. */
let main = null, pan = null, lnk = null, tgs = null;
/** COL as a real column index, or null for the whole-row look — which is what a
 * column outside the table IS.  The real one's `cellCol', mirrored here because
 * the shell's cell movement hands the index one past an end straight back. */
const cellCol = (cols, col) => {
  if (col === null || col === undefined) return null;
  const at = Math.trunc(col);
  return at >= 0 && at < cols.length ? at : null;
};
/** Set by `bare', `pageless', `sortless' and `crumbless': this asset never had
 * those calls, remounts included. */
let markless = false, pagerless = false, sortnone = false, crumbless = false;
/**
 * One mount, with its own everything.  OWN is the row list it keeps for itself,
 * or null for the instance whose rows are the store's.
 */
const makeMount = (host, view, options, own) => {
  const o = options || {};
  const m = {
    own,
    // The columns the SHELL declared, per instance, rather than a second copy
    // spelled here: a column added to either mount reaches the stub for free,
    // where a hardcoded pair would silently go on agreeing.  The table's mount
    // is handed the store's view, so `recolumn' reaches it through a remount.
    cols: (view || {}).columns || [],
    // The chain in force is the QUERY's where it names any `sort:' token, and
    // the view's declared `sort' where it names none — the renderer's own rule,
    // mirrored because the shell's `^' and its canned views both rest on it.
    _seedSort: (() => {
      const named = sortTokensIn(o.initialQuery || "");
      const d = (view || {}).sort;
      sortChain = named.length ? named
        : (Array.isArray(d) ? d : d ? [d] : [])
            .map((k) => ({ column: k.column, ascending: k.ascending !== false }));
      return null;
    })(),
    held: o.initialQuery || "",
    marksOn: o.marks === true,
    flagsOn: o.flags === undefined ? o.marks === true : o.flags === true,
    hintsOn: o.actionHints !== false,
    flagHelp: o.flagHelp || "",
    // The page size is the mount's, the way the real one takes it, so a script
    // that never asks for pages gets the one the shell always requests.
    pageSize: o.pageSize || 0,
    // WHERE THE CURSOR IS, in the two terms the renderer keeps it in: the visual
    // place, and the row that was standing there.  Both, because rows go away
    // under a mount — spliced out by a frame, dropped by an answer — and the
    // real one's `keepSelection' keeps the ROW while it is still on the page and
    // falls back to the PLACE, clamped, when it is not.
    cursor: 0, rowId: null, selCol: null, pageAt: 0,
    marks: new Set(), flags: new Set(), crumbs: [],
    // The pin button-badge: whether the badge is on, and the click the
    // consumer wired.  `pinclick' is the act that presses it.
    pinned: !!o.pinned, onPin: typeof o.onPin === "function" ? o.onPin : null,
    onFilter: typeof o.onFilter === "function" ? o.onFilter : null,
  };
  /** Every row this mount holds: its own, or the store's. */
  const all = () => (m.own ? m.own : rows);
  const pageMax = () =>
    (m.pageSize ? Math.max(1, Math.ceil(all().length / m.pageSize)) : 1);
  /** The rows on show: one page's worth, or the whole set when there are none.
   * The page is CLAMPED rather than reset, the way the renderer clamps it, so a
   * set that shrank out from under the last page shows the new last one. */
  const onPage = () => {
    if (!m.pageSize) return all();
    m.pageAt = Math.max(0, Math.min(m.pageAt, pageMax() - 1));
    return all().slice(m.pageAt * m.pageSize, (m.pageAt + 1) * m.pageSize);
  };
  /**
   * `keepSelection' verbatim, and the reason the pair above is kept rather than
   * an index alone: a row still on the page keeps the cursor whatever moved
   * around it, and one that went takes its PLACE with it, clamped.  The place
   * is the last index something explicitly landed on — it is deliberately NOT
   * re-derived while the row is still there, which is what makes a run of rows
   * going from ABOVE point land the fallback lower than the row point was on.
   * Called wherever the real one renders with the rows moved: `setRows' and the
   * two frame ops.
   */
  // A MOUNT NOTHING HAS SELECTED IN HAS NO SELECTION: `rowId' null is the real
  // one's `state.selected === null', and every answer below reads it as such —
  // `keepSelection' returns at the guard, `indexOfSelected' answers -1 and
  // `getSelection' answers a null id.  The renderer selects nothing of its own
  // (`selectFirstVisible' has one caller and it is the filter box handing
  // over), so a page that wants a cursor on its first row has to land one.
  // Everything below the guard is `keepSelection' line for line.
  const keep = () => {
    if (m.rowId === null) return;
    const on = onPage();
    // Emptied: the row, the column and the PLACE all go, and the place going is
    // what makes the next set land on row 0 rather than where this one stood.
    if (!on.length) { m.rowId = null; m.selCol = null; m.cursor = -1; return; }
    if (on[m.cursor] && on[m.cursor].id === m.rowId) return;
    if (on.some((r) => r.id === m.rowId)) return;
    m.cursor = Math.max(0, Math.min(m.cursor, on.length - 1));
    m.rowId = on[m.cursor].id;
  };
  /** Where the cursor sits now — `indexOfSelected', -1 with nothing selected,
   * falling back to the clamp for the one state the real one cannot be in: rows
   * moved by an ACT, which the store shares with this mount and no call
   * announces. */
  const held = () => {
    const on = onPage();
    if (m.rowId === null || !on.length) return -1;
    if (on[m.cursor] && on[m.cursor].id === m.rowId) return m.cursor;
    const i = on.findIndex((r) => r.id === m.rowId);
    return i !== -1 ? i : Math.max(0, Math.min(m.cursor, on.length - 1));
  };
  /** Put the cursor on index I of the page in hand, remembering the row. */
  const sit = (i) => {
    const on = onPage();
    m.cursor = on.length ? Math.max(0, Math.min(i, on.length - 1)) : 0;
    m.rowId = on.length ? on[m.cursor].id : null;
  };
  /**
   * Turn to page TO, counting from zero, landing the cursor on the end it
   * arrives at — FIRST says which.  The column rides across untouched, which is
   * what lets the shell read it back rather than carry it.  False when there is
   * no such page, which is how a stop at either end is told from a turn.
   */
  const pageTo = (to, first) => {
    const at = Math.max(0, Math.min(pageMax() - 1, to));
    if (at === m.pageAt) return false;
    m.pageAt = at;
    sit(first ? 0 : onPage().length - 1);
    return true;
  };
  m.onPage = onPage;
  m.at = held;
  m.sit = sit;
  m.handle = {
    // The root the mount drew into, which the real handle publishes and the
    // sheet's edit overlay reads a row's box through.  Nothing here has a
    // layout, so the query finds no row and the overlay stays where it was —
    // the geometry is the one thing this harness cannot stand in for.
    el: host || { querySelector: () => null },
    // The table's `setRows' is a count: its rows are the store's and an act is
    // what moves them.  The panel's are the shell's model, and the whole of what
    // the panel shows, so that instance keeps them.  Either way the cursor is
    // kept the way the real one keeps it — `renderRows' runs `keepSelection'
    // first, whatever the rows were handed to it.
    setRows: (list) => {
      if (m.own) {
        m.own = (list || []).slice();
        if (m === tgs) tsets += 1; else if (m === pan) psets += 1;
      } else { sets += 1; paints.push((list || []).length); }
      keep();
    },
    // The row ops a socket frame carries, which is the whole of what an
    // unfiltered client applies without asking the server again.  The table's
    // rows ARE the store's, so these move the store; `keep' is what holds the
    // cursor afterwards, which is the renderer's own `keepSelection'.
    // Recorded, because a shell that landed the right row without ever splicing
    // would read the same off the rows alone.
    upsertRow: (row) => {
      spliced.push(`upsert ${row.id}`);
      const list = all(), at = list.findIndex((r) => r.id === row.id);
      if (at === -1) list.push(row); else list[at] = row;
      keep();
    },
    deleteRow: (id) => {
      spliced.push(`delete ${id}`);
      const list = all(), at = list.findIndex((r) => r.id === id);
      if (at !== -1) list.splice(at, 1);
      m.marks.delete(id);   // the row is gone; a mark on it would outlive it
      m.flags.delete(id);
      keep();
    },
    getQuery: () => m.held,
    getRows: () => all().slice(),
    setQuery: (q) => { m.held = String(q == null ? "" : q).trim(); },
    setPinned: (on) => { m.pinned = !!on; },
    stripLastToken: () => {
      if (!m.held) return false;
      m.held = tokensOf(m.held).slice(0, -1).join(" ");
      return true;
    },
    // The selection is the renderer's, both halves of it, and the shell reads
    // the row id back out of here to materialize one.
    getSelection: () => {
      const at = held();
      return { id: at === -1 ? null : onPage()[at].id, col: m.selCol };
    },
    getVisible: () => onPage(),
    // Clamped, never wrapped, and false at the end — which is what tells the
    // shell that a mark on the last row has nowhere to walk to.  From NO
    // selection it lands on the end it is stepping away from, the way the real
    // one does with `state.selected' null: forward takes the first row, back
    // the last.
    selectStep: (step) => {
      const on = onPage();
      if (!on.length) return false;
      const at = held();
      if (at === -1) { sit(step < 0 ? on.length - 1 : 0); return true; }
      const to = at + step;
      if (to < 0 || to >= on.length) return false;
      sit(to);
      return true;
    },
    // A row of the page in hand, and the column to land in.  Null is a
    // WHOLE-ROW selection, and so is a column index OUTSIDE the table — the
    // real one's `cellCol' reads both the same way, which is what makes
    // walking off the last cell a landing rather than a wall.  False for a row
    // this page is not showing; the row is what the bool answers about.
    select: (id, col) => {
      const at = onPage().findIndex((r) => r.id === id);
      if (at === -1) return false;
      sit(at);
      m.selCol = cellCol(m.cols, col);
      return true;
    },
    // The pager, landing the cursor on the end it arrives at — the new page's
    // first row going forward, its last coming back.
    nextPage: () => pageTo(m.pageAt + 1, true),
    previousPage: () => pageTo(m.pageAt - 1, false),
    pageInfo: () => {
      const size = m.pageSize || all().length;
      return { page: m.pageAt + 1, pages: pageMax(),
               from: all().length ? m.pageAt * size + 1 : 0,
               to: Math.min(all().length, (m.pageAt + 1) * size), total: all().length };
    },
    // Marks are the renderer's, keyed by id.
    toggleMark: (id) => {
      const on = !m.marks.has(id);
      if (on) m.marks.add(id); else m.marks.delete(id);
      return on;
    },
    getMarked: () => [...m.marks],
    clearMarks: () => m.marks.clear(),
    markedCount: () => m.marks.size,
    markAll: () => { for (const r of all()) m.marks.add(r.id); },
    // Archive flags, keyed by id the way marks are: `d' puts one on and a
    // second `d' on the same row is what archives it.
    flagRow: (id) => m.flags.add(id),
    unflagRow: (id) => m.flags.delete(id),
    getFlagged: () => [...m.flags],
    clearFlags: () => m.flags.clear(),
    // What the renderer's palette does: the overlay goes up and its field
    // takes focus, which is the whole of what the shell can see of it.
    openFilter: () => { raises += 1; field("filter").focus(); },
    // A PRODUCER's sort: it states an order and writes no query.  Recorded
    // rather than performed — the ORDER is the renderer's and TableView's own
    // suite is where it is tested — and nothing in the shell calls it any more,
    // a canned view carrying its order as a `sort:' token instead.
    sortBy: (column, ascending) => { sorted = { column, ascending }; sortCalls += 1;
      sortChain = [{ column, ascending }]; },
    // The promotion rule verbatim: head ascending, dedup below; the leader
    // flips alone.  getSort answers copies, the way the renderer documents.
    //
    // And it WRITES THE QUERY, which is the half the shell can see: the new
    // chain replaces whatever `sort:' tokens were applied and the query is
    // delivered, so a press arrives at `onFilter' as an ordinary commit.
    //
    // `sortable' is enforced HERE, the way the renderer enforces it, and the
    // answer is what the shell reads its refusal off: false when the column
    // opts out, true when the chain moved.
    sortPromote: (column) => {
      const col = (m.cols || []).find((c) => c.key === column);
      if (!col || col.sortable !== true) return false;
      const head = sortChain[0];
      if (head && head.column === column) {
        head.ascending = head.ascending === false;
      } else {
        sortChain = [{ column, ascending: true }]
          .concat(sortChain.filter((k) => k.column !== column));
      }
      sorted = { column: sortChain[0].column, ascending: sortChain[0].ascending };
      sortCalls += 1;
      m.held = withSort(m.held, sortChain);
      if (o.onFilter) o.onFilter(m.held);
      return true;
    },
    getSort: () => sortChain.map((k) => ({ column: k.column, ascending: k.ascending })),
    setSort: (chain) => { sortChain = (chain || []).map((k) => ({ column: k.column, ascending: k.ascending !== false })); },
    // The drill-down trail.  `popCrumb' pops and RETURNS — it never applies —
    // because whoever owns the fetching owns what a query means, which is the
    // whole reason the shell has a ladder to walk rather than the renderer.
    // `getCrumbs' answers with copies, so a reader cannot move the strip.
    setCrumbs: (list) => {
      m.crumbs = (Array.isArray(list) ? list : [])
        .filter((c) => c && typeof c === "object")
        .map((c) => ({ label: String(c.label || ""), query: String(c.query || "") }));
    },
    getCrumbs: () => m.crumbs.map((c) => ({ label: c.label, query: c.query })),
    pushCrumb: (c) => { m.handle.setCrumbs(m.crumbs.concat([c])); return m.crumbs.length; },
    popCrumb: () => (m.crumbs.length ? m.crumbs.pop() : null),
  };
  return m;
};
main = makeMount(null, null, {}, null);
globalThis.TableView = {
  // WHICH mount this is, by the element it was given: the sheet's panel hosts
  // itself in `#mptable', the link popup in `#ltable', the tags popup in
  // `#ttable' and the table in `#app'.  Told apart by the host rather than by
  // call order, since a remount builds a second table long after any of the
  // others went up.
  mount: (host, view, options) => {
    const panel = host === field("mptable"), popup = host === field("ltable");
    const tagbox = host === field("ttable"), maker = host === field("cfbox");
    const states = host === field("cstates");
    const inst = makeMount(host, view, options,
                           panel || popup || tagbox || maker || states ? [] : null);
    if (panel) { pmounts += 1; pan = inst; }
    else if (popup) { lmounts += 1; lnk = inst; }
    else if (tagbox) { tmounts += 1; tgs = inst; }
    else if (maker) { cmounts += 1; cmp = inst; }
    else if (states) { smounts += 1; sts = inst; }
    else { mounts += 1; main = inst; paints.push(((view || {}).rows || []).length); }
    if (markless) strip(inst.handle, MARK_CALLS);
    if (pagerless) strip(inst.handle, PAGE_CALLS);
    if (sortnone) strip(inst.handle, SORT_CALLS);
    if (crumbless) strip(inst.handle, CRUMB_CALLS);
    return inst.handle;
  },
  // A TOKEN SPLIT, not the renderer's grammar: whitespace-separated, `-'
  // negating and the first `:' or `=' cutting a key off — which is enough for
  // the two readers on this page (the parity tripwire's free-text scan and the
  // capture form's tag seed) and stops the stub answering `no tokens' to
  // questions the real renderer answers with some.
  parseQuery: (q) => String(q || "").split(/\s+/).filter(Boolean).map((raw) => {
    const negated = raw.startsWith("-");
    const body = negated ? raw.slice(1) : raw;
    const quoted = body.startsWith("\"");
    const at = quoted ? -1 : body.search(/[:=]/);
    return at === -1
      ? { key: null, value: body, negated, quoted }
      : { key: body.slice(0, at), value: body.slice(at + 1), negated, quoted };
  }),
  displayText: (s) => String(s || ""),
};
/** The mark calls off the live handle: what an older table-view.js looks like. */
const MARK_CALLS = [ "toggleMark", "getMarked", "clearMarks", "markedCount"
                   , "markAll", "flagRow", "unflagRow", "getFlagged", "clearFlags" ];
/** And the pager's, which an asset that old has none of either. */
const PAGE_CALLS = ["nextPage", "previousPage", "pageInfo"];
/** And the programmatic sort, which the agenda asks for. */
const SORT_CALLS = ["sortBy", "sortPromote", "getSort", "setSort"];
/** And the crumb trail, which `@' needs before it will drill at all. */
const CRUMB_CALLS = ["setCrumbs", "getCrumbs", "pushCrumb", "popCrumb"];
const strip = (h, names) => { for (const name of names) delete h[name]; };
let cmp = null, cmounts = 0;
/** And the settings sheet's states table, its fifth mount. */
let sts = null, smounts = 0;
/** An older asset is one asset: every mount loses the calls it never had. */
const stripLive = (names) => {
  for (const inst of [main, pan, lnk, tgs, sts]) if (inst) strip(inst.handle, names);
};
// The one thing a key here does that leaves nothing on the page: the tab `o'
// opens.  Recorded whole — the target, the tab name and the features — since
// `noopener' is half of what makes following a link safe.
globalThis.open = (url, target, features) => {
  opened.push({ url, target, features });
  return null;   // what a browser answers for a `noopener' window
};
// A real one, in memory: the theme is a stored preference and "it persisted" is
// a question about what is in here after the pick, which a stub swallowing every
// write cannot answer.
const stored = {};
for (const pair of (store || "").split(",").filter(Boolean)) {
  const at = pair.indexOf("=");
  stored[pair.slice(0, at)] = pair.slice(at + 1);
}
globalThis.localStorage = {
  getItem: (k) => (Object.prototype.hasOwnProperty.call(stored, k) ? stored[k] : null),
  setItem: (k, v) => { stored[k] = String(v); },
  // A preference EMPTIED is one that is not there, which a stub writing `""'
  // could not tell from one set to the empty string.
  removeItem: (k) => { delete stored[k]; },
};
globalThis.matchMedia = () => ({ matches: false, addEventListener: () => {} });

// One element that answers to anything: the boot reads and writes chrome this
// harness has no opinion about, and the keymap blob is the one thing it has to
// hand back for real.
const KEYS = fs.readFileSync(dir + "/keys.json", "utf8");
// And the configuration blob the glue boots from — the page's second JSON
// script element, extracted the same way.
const CFGJSON = fs.readFileSync(dir + "/cfg.json", "utf8");
const node = new Proxy(
  {},
  {
    get: (_target, key) =>
      key === "textContent" || key === "className" || key === "value" ? ""
        : key === "scrollTop" || key === "clientHeight" || key === "scrollHeight" ? 0
        : () => node,
    set: () => true,
  }
);
// The few elements whose contents are the answer to a question asked here: the
// sheet's two panes and its one-word state, and the renderer's filter field.  A
// proxy answering "" to everything cannot hold text a restore is checked
// against, cannot hold a tree the property panel is built into, and
// `document.activeElement' is what tells a raised palette from a committed
// query.
let active = null;
const fields = {};
// The tag matters: `typing()' reads it off `document.activeElement' to decide
// whether a key belongs to the table or to whatever has focus.
const TAGS = { mtext: "textarea", filter: "input", pinput: "input",
               dtin: "input", dtext: "textarea",
               pkey: "input", pval: "input",
               tname: "input", themesel: "select",
               ltitle: "input", lurl: "input",
               ctarget: "input", clog: "input",
               // The capture form: the tag field and the line; the template's
               // grown fields are page-made nodes the acts reach through the
               // focus.
               ktag: "input", ktext: "textarea",
               // The keywords panel: one select over the layers and one box
               // showing the selected one's lines.
               clayer: "select", ctext: "textarea", ctpl: "textarea" };
/**
 * Enough of a `CSSStyleDeclaration': a named property is an ordinary field the
 * page writes and reads back (the palette's badge hues), and a CUSTOM property
 * goes through the pair a real one answers to — which is how the log's cap is
 * written, as a number onto the element rather than a length.
 */
const styleOf = () => ({
  custom: {},
  setProperty(name, value) { this.custom[name] = String(value); },
  getPropertyValue(name) { return this.custom[name] || ""; },
});
/** A stand-in element, enough of one for the page to build its own chrome in. */
// Every `scrollIntoView' the page made, oldest first.  See the stub below.
const scrolls = [];
const make = (tag) => {
  const e = {
    tagName: String(tag).toUpperCase(),
    value: "", className: "", placeholder: "", spellcheck: false,
    style: styleOf(), dataset: {}, children: [],
    scrollTop: 0, clientHeight: 0, scrollHeight: 0,
    focus() { active = this; },
    blur() { if (active === this) active = null; },
    // Kept rather than dropped: the value palette narrows on its field's own
    // `input' event, and the property panel grows a row on one — neither of
    // which a document-level press can stand in for.
    on: {},
    addEventListener(type, fn) { (this.on[type] = this.on[type] || []).push(fn); },
    fire(type, event) { for (const fn of this.on[type] || []) fn(event); },
    appendChild(child) { child.up = this; this.children.push(child); return child; },
    // Nothing here has a layout or a real tree, so a selector finds nothing —
    // which is the honest answer, and the one every geometry read is written to
    // survive.
    querySelector: () => null,
    // What the log's ring drops the oldest line with.
    removeChild(child) {
      const at = this.children.indexOf(child);
      if (at !== -1) this.children.splice(at, 1);
      return child;
    },
    select() {},
    // Geometry is beyond this stub — nothing here has a layout, so whether an
    // element IS out of view can never be answered.  What can be answered is
    // whether the page ASKED, which is the half that is this page's: the call
    // is recorded with the class of what it was made on and the options it
    // carried, and the tests pin that.  Same caveat as the overlay's placing.
    scrollIntoView(opts) { scrolls.push({ className: this.className, opts }); },
  };
  // A real element's `classList', which the page uses where it means "set one
  // class, keep the rest" — the sheet's shape flag riding beside its size tier.
  // The document element models one already; this is the same three calls on
  // every element, since a stub that answered "" to `toggle' would silently
  // drop whichever class the page was keeping.
  e.classList = {
    contains: (name) => String(e.className).split(" ").indexOf(name) !== -1,
    add: (name) => { if (!e.classList.contains(name)) e.className = `${e.className} ${name}`.trim(); },
    remove: (name) => {
      e.className = String(e.className).split(" ").filter((c) => c !== name).join(" ");
    },
    toggle: (name, force) => {
      const on = force === undefined ? !e.classList.contains(name) : !!force;
      if (on) e.classList.add(name); else e.classList.remove(name);
      return on;
    },
  };
  // The real one drops every child when it is set, which is how the panel is
  // cleared before it is drawn again.
  let text = "";
  // Every value the text was SET to, in order.  A pill that is last-writer-wins
  // on screen — the echo — leaves no trace of a second identical write, so a
  // case asking "did this run twice" has nothing to read; the history is that
  // trace, and it costs one push per set.
  e.wrote = [];
  Object.defineProperty(e, "textContent", {
    // The real one is the whole SUBTREE's text, which is what an element drawn
    // as segments — text, link, text — reads as.  Setting it drops the
    // children, so the two halves never double-count.
    get: () => text + e.children.map((c) => c.textContent).join(""),
    set: (v) => { text = String(v); e.wrote.push(text); e.children.length = 0; },
  });
  return e;
};
const field = (id) => (fields[id] = fields[id] || make(TAGS[id] || "div"));
const STATEFUL = [ "mtext", "mnote", "mfile", "modal", "mprops", "mlog", "sheet"
                 , "mwhere"
                 // The structured document, which is the page's OWN widget
                 // rather than a mount: `dlist' is the tree it draws its
                 // elements into, and the four below are the two edit overlays
                 // laid over the element at point.
                 , "mdoc", "dlist", "dtitle", "dtin", "dpara", "dtext"
                 // And the property panel, which IS a mount: `mptable' is the
                 // element it is given, and the three below are its own edit
                 // overlay laid over the row at point.
                 , "mptable", "pedit", "pkey", "pval"
                 // The value palette: its list is a tree of key tokens and
                 // underlined words, so it has to hold one.
                 , "echo", "prompt", "phead", "pinput", "pbox", "plist", "pfoot"
                 // The link popup, which is a MOUNT like the panel: `ltable' is
                 // the element it is given, `lpane' the box the edit overlay is
                 // placed inside, `ledit'/`ltitle'/`lurl' the edit itself, and
                 // the two lines around them the only chrome it draws.
                 , "links", "lhead", "lpane", "ltable", "lfoot"
                 , "ledit", "ltitle", "lurl"
                 // And the tags popup, which is a mount and an edit overlay:
                 // `ttable' is the element, `tpane' the box the overlay is
                 // placed inside, and `tedit'/`tname' the rename itself.
                 , "tags", "thead", "tpane", "ttable", "tfoot", "tedit", "tname"
                 // The capture form: the one popup `+' raises whole — its
                 // backdrop, head and foot, the tag field with its narrowed
                 // list, the container the template's fields grow into, and
                 // the line.
                 , "capture", "khead", "ktag", "klist", "kfields", "ktext", "kfoot"
                 // The settings sheet's default-view composer host.
                 , "cfbox"
                 // The settings sheet: its state, its panel frames, the fields
                 // of the general panel — the two tree-wide lines, which are
                 // `system.org''s and ride in that layer's write, plus the log
                 // knob, which is this page's own preference — and the keywords
                 // panel's select, box, label and refusal line.
                 , "config", "cnote", "ceff", "csecs", "ctarget"
                 // `ctpl' is the layer's capture template — a REGION of the
                 // same file, kept on the layer like its cycle and riding in
                 // the same write.
                 , "clog", "clayer", "ctext", "ctpl", "clab", "clerr"
                 // Which saved view the composer is standing on: a select the
                 // page fills off the server's own list, like the layer one.
                 , "cwhich", "ctabs", "chues", "cstates"
                 // The states table's edit overlay and its three fields.
                 , "sedit", "sname", "sgroup", "shue"
                 // The event strip: a line per entry, each a row of spans, so it
                 // has to hold a tree rather than answer "" to everything.
                 , "log"
                 // The sheet's theme select, which has to be a real element for
                 // the focus it holds to be observable.
                 , "themesel" ];
// The document element, which is where the stale wash lands and where the theme
// pins its attribute.  A real element rather than the catch-all proxy, because
// the wash IS a class going on and coming off and a stub that swallows every
// write can show none of it.  Every transition of that class is recorded, so a
// wash that armed and cleared reads differently from one that never armed.
const root = make("html");
const washed = [];
root.classList = {
  contains: (name) => root.className.split(" ").indexOf(name) !== -1,
  toggle: (name, force) => {
    const has = root.classList.contains(name);
    const on = force === undefined ? !has : !!force;
    if (on === has) return on;
    root.className = on ? `${root.className} ${name}`.trim()
                        : root.className.split(" ").filter((c) => c !== name).join(" ");
    if (name === "stale") washed.push(on ? "on" : "off");
    return on;
  },
};
// The page's own key dispatch, kept so a press can be delivered to it.
const pressed = [];
const released = [];
globalThis.document = {
  getElementById: (id) =>
    id === "keys" ? { textContent: KEYS }
      : id === "cfg" ? { textContent: CFGJSON }
      : STATEFUL.indexOf(id) === -1 ? node : field(id),
  querySelector: (sel) => (sel === "#app .tv-filter" ? field("filter") : null),
  querySelectorAll: () => [],
  createElement: (tag) => make(tag),
  addEventListener: (type, handler) => {
    if (type === "keydown") pressed.push(handler);
    if (type === "keyup") released.push(handler);
  },
  getSelection: () => null,
  get activeElement() { return active; },
  documentElement: root,
  body: node,
};
globalThis.window = globalThis;
globalThis.addEventListener = () => {};

eval(fs.readFileSync(dir + "/shell.js", "utf8"));

// A `C-' prefix is the chord the page's own `keyName' spells that way, so a
// sequence like `C-c C-t' is two of these and needs no other notation here.
// `S-' is the shift held with it — `S-Tab' is the crossing back out of the
// sheet's property panel, which the page tells from `Tab' by the modifier
// alone.
//
// A `%CODE' tail is the PHYSICAL key under the character, which is the one
// thing a layout changes: `т%KeyN' is the key a Latin layout writes `n' on,
// pressed on a Cyrillic one, and `S-В%KeyD' is that key's shifted half.  A name
// without one carries no `code' at all — the fallback a browser that sends none
// leaves the page with, and what every other press here is.
//
// Whether the dispatch CLAIMED a key is recorded, because that is the half of
// the reserved-chord rule behaviour can otherwise not show: a chord the page
// leaves to the browser and one it takes both look like nothing happening.  It
// is recorded under the name the script PRESSED, tail and all.
const press = (name, repeating, held) => {
  // A tail wants both halves, so a `%' with nothing either side of it is the
  // key it spells rather than a separator.
  const cut = name.indexOf("%"), tailed = cut > 0 && cut < name.length - 1;
  const code = tailed ? name.slice(cut + 1) : undefined;
  const spelled = tailed ? name.slice(0, cut) : name;
  const ctrl = spelled.startsWith("C-"), shift = spelled.startsWith("S-");
  // Acts split on whitespace, so the space BAR is spelled `Space' here and
  // cooked to the " " a browser actually sends — the glue never learns a
  // key name no browser speaks.
  const bare = ctrl || shift ? spelled.slice(2) : spelled;
  const event = {
    key: bare === "Space" ? " " : bare,
    code,
    ctrlKey: ctrl, altKey: false, metaKey: false, shiftKey: shift,
    repeat: !!repeating, target: node,
    // The DOM's own record of "a listener has handled this", which the later
    // listeners on one document read to stay off a key an earlier one took.
    defaultPrevented: false,
    preventDefault: () => { prevented.push(name); event.defaultPrevented = true; },
  };
  for (const handler of pressed) handler(event);
  // And the browser's OWN default for the one key whose default a field needs:
  // a `Backspace' no listener claimed erases a character out of whatever field
  // has the focus.  Without it "the page left this key to the field" is
  // indistinguishable from "nothing happened", which is exactly the rule a
  // popup's nav-mode DEL has to be told apart from.
  if (spelled === "Backspace" && !event.defaultPrevented && active
      && (active.tagName === "INPUT" || active.tagName === "TEXTAREA"))
    active.value = String(active.value).slice(0, -1);
  // The KEYUP the browser always sends and the page's derived-repeat set
  // waits for.  A HELD key sends none — which is what `stuck:' models: the
  // native window's GTK quirk delivers auto-repeat keydowns with `repeat'
  // unset, and the derivation must catch the second press by the missing
  // release alone.
  if (!held)
    for (const handler of released)
      handler({ key: event.key, code: event.code });
};
const prevented = [];

// The store moving is a new tag: a client holding the old one is answered with
// a body rather than a 304, which is the reconnect that has rows to apply.
const step = () => { tag = `"t${Number(tag.slice(2, -1)) + 1}"`; };
/** TEXT typed into the settings sheet's fixed field ID, the sheet being open —
 * a closed one shows no field, so a script that types into one means nothing. */
const typeSetting = (id, text) => {
  if (field("config").className !== "on")
    throw new Error(`the settings sheet is not open: ${id}`);
  typed(field(id), text);
};
/** TEXT typed into BOX, event and all — which is what a reader does to a field
 * and what the widgets narrowing on one are listening for. */
const typed = (box, text) => {
  box.value = text;
  box.fire("input", { target: box });
};
/**
 * Type into one of the document's two edit overlays.  A closed one has no
 * fields, so a script that types without pressing RET first is typing into
 * nothing on a real page: say so rather than write where no reader could have.
 */
const typeIn = (box, which, text) => {
  if (field(box).className !== "on")
    throw new Error(`the document has no ${box} open: ${which}`);
  typed(field(which), text);
};
/**
 * Type into the property panel's edit overlay: ARG is `INDEX=TEXT', INDEX being
 * the row the script means.  A closed panel has no fields, and an overlay open
 * over another row is a script that means nothing on a real page — say so rather
 * than write where no reader could have.
 */
const typeOver = (which, arg) => {
  const at = arg.indexOf("=");
  if (field("pedit").className !== "on")
    throw new Error(`no panel row is open for editing: ${which}:${arg}`);
  if (String(patAt()) !== arg.slice(0, at))
    throw new Error(`panel row ${patAt()} is open, not ${arg}`);
  typed(field(which), arg.slice(at + 1));
};
/**
 * Type into the link popup's edit overlay.  A closed overlay has no fields, so a
 * script that types without pressing RET first is typing into nothing on a real
 * page: say so rather than write where no reader could have.
 */
const typeLink = (which, text) => {
  if (field("ledit").className !== "on")
    throw new Error(`no link is open for editing: ${which}`);
  typed(field(which), text);
};
/**
 * What INST is showing: KEYS' cells, one array per row.  Read off the MOUNT
 * rather than off any DOM, because the two model-owning mounts ARE their models
 * — the shell hands each a row list and that list is the whole of what it shows.
 * An open row is not in it: the overlay holds the edit and the model holds the
 * committed text, which is what makes a commit the only thing that means yes.
 */
/** The keywords panel on screen, which is where its controls are typeable. */
const onKeywords = () => {
  const tab = field("ctabs").children.find((t) => t.textContent === "keywords");
  if (tab && tab.className !== "ctab on") tab.fire("click", {});
};
const cellsOf = (inst, keys) =>
  (inst ? inst.own.map((r) => keys.map((k) => r.cells[k])) : []);
/** Which row wears INST's cursor, and -1 when there is no such mount yet — or
 * when nothing has been selected in it, which its own answer already is. */
const curOf = (inst) => (inst ? inst.at() : -1);
/** What INST was mounted with and what it keeps, under PREFIX: its columns, the
 * four options a popup is handed, and its two id sets.  Every mount answers all
 * eight, and one that is not up answers each field's own empty. */
const mountFields = (prefix, inst) => ({
  [`${prefix}cols`]: inst ? inst.cols : [],
  [`${prefix}marks`]: inst ? inst.marksOn : null,
  [`${prefix}flags`]: inst ? inst.flagsOn : null,
  [`${prefix}hints`]: inst ? inst.hintsOn : null,
  [`${prefix}page`]: inst ? inst.pageSize : null,
  [`${prefix}flagHelp`]: inst ? inst.flagHelp : "",
  [`${prefix}marked`]: inst ? [...inst.marks] : [],
  [`${prefix}flagged`]: inst ? [...inst.flags] : [],
});
/**
 * THE STRUCTURED DOCUMENT, read off what it DREW.  It is the page's own widget
 * rather than a mount, so there is no model to ask: `#dlist' holds one element
 * per row, each wearing its KIND as a class and holding its parts as spans, and
 * what a reader has in front of them is exactly that.
 *
 * An element reads as `[KIND, ...parts]' — a headline line as its four cells, a
 * planning or property row as its key and its value, a paragraph as its text.
 */
const kindOf = (cls) => String(cls).split(" ")
  .filter((c) => c.startsWith("d-")).map((c) => c.slice(2)).join(":");
const wears = (e, cls) => String(e.className).split(" ").indexOf(cls) !== -1;
/** THE WALK, FLATTENED OUT OF THE DRAW.  A composite is drawn ONCE with its
 * leaves inside it and a leaf with children draws THEM inside itself — the
 * grain ladder — so the elements are a tree on screen and a flat sequence to
 * the cursor.  Flattened recursively, in draw order, which is the builder's
 * emission order. */
const flatRows = () => {
  const out = [];
  const walk = (e) => {
    for (const kid of e.children)
      if (wears(kid, "de")) { out.push(kid); walk(kid); }
  };
  walk(field("dlist"));
  return out;
};
/** Which row each stop hangs under, by place in the walk — the IMMEDIATE
 * parent up the ladder, -1 for a top-level stop. */
const ownerOf = () => {
  const rows = flatRows();
  return rows.map((row) => {
    for (let e = row.up; e; e = e.up)
      if (rows.indexOf(e) !== -1) return rows.indexOf(e);
    return -1;
  });
};
/** Every drawn piece of ROW's text, as `CLASS:TEXT' in draw order — `dt' for
 * plain text and `dl' for a link, so a test can read the interleaving.  An
 * element with no link in it is drawn as text outright and has no pieces. */
const segsOf = (row) => {
  const out = [];
  const walk = (e) => {
    for (const kid of e.children) {
      if (wears(kid, "dt") || wears(kid, "dl"))
        out.push(`${wears(kid, "dl") ? "dl" : "dt"}:${kid.textContent}`);
      else if (!wears(kid, "de")) walk(kid);
    }
  };
  walk(row);
  return out;
};
const docRows = () => flatRows().map((row) =>
  [kindOf(row.className)].concat(row.children
    .filter((p) => !wears(p, "de")).map((p) => p.textContent)));
/** Which element wears the cursor, and which of its CELLS — -1 for neither.
 * Counted over the `dc' parts alone: a headline line opens with its org-cleaned
 * stars, which are chrome ahead of the first cell rather than a cell, so `f'/`b'
 * walk straight past them and so does this. */
const docAt = () => flatRows().findIndex((row) => wears(row, "dat"));
const docCell = () => {
  const row = flatRows()[docAt()];
  if (!row) return -1;
  return row.children.filter((p) => wears(p, "dc")).findIndex((p) => wears(p, "don"));
};
/** And which elements wear a deletion flag, by their place in the document. */
const docFlagged = () => flatRows()
  .map((row, i) => (wears(row, "dfl") ? i : -1))
  .filter((i) => i !== -1);
/** The property panel: a [key, value] pair per row, and where its cursor is. */
const panel = () => cellsOf(pan, ["key", "value"]);
const patAt = () => curOf(pan);
/**
 * Which field has the focus, named the way an act names one: `mtext' for raw
 * mode's textarea, and the document's or a popup's overlay fields by their own
 * names.  Nothing focused is the state the document holds the keys in, which is
 * what leaves every printable key free.
 */
const FOCUSABLE = ["mtext", "dtin", "dtext", "ltitle", "lurl", "tname",
                   "pinput", "ktag", "ktext"];
const focused = () => {
  if (!active) return "";
  // The panel's two fields carry the row they are laid over, since the overlay
  // is ONE pair over whichever row the panel's cursor is on.
  const which = active === field("pkey") ? "pkey"
    : active === field("pval") ? "pval" : "";
  if (which) return `${which}:${patAt()}`;
  return FOCUSABLE.find((id) => active === field(id)) || "";
};
/** Everything under E with CLS, by class the way `docRows' reads the document:
 * the producer labels each part, so one added later cannot be mistaken for
 * another. */
const parts = (e, cls) =>
  e.children.filter((x) => x.className.split(" ").indexOf(cls) !== -1);
/** A bare word where an entry is read: the header's column names, and the line
 * the palette stands on while the resolution is out. */
const asWord = (word) => ({ key: "", word, color: "", hint: "", mark: "" });
/**
 * One palette entry as it is drawn: the key token it claimed, its word spelled
 * with the BOLD letter in brackets where it sits (`DELEGAT[E]D'), and the
 * colour read back off the inline style, which is where the badge's own hue is
 * written.
 */
const paletteEntry = (e) => {
  const token = parts(e, "pk")[0], word = parts(e, "pw")[0], aside = parts(e, "pt")[0];
  const hot = word.children.find((p) => p.tagName === "B");
  return {
    // Empty for every entry but the one whose key names no position in a word:
    // the letter is marked INSIDE the keyword and there is no token column.
    key: token ? token.textContent : "",
    word: word.children.length
      ? word.children.map((p) => (p.tagName === "B" ? `[${p.textContent}]`
                                                    : p.textContent)).join("")
      : word.textContent,
    color: word.style.color || "",
    // The rule under the claimed letter, which takes that state's badge hue —
    // the only place a reader is told which key commits now that the token
    // column is gone.
    mark: hot ? hot.style.textDecorationColor || "" : "",
    // The muted aside: the tag palette's partial count.  Empty where the entry
    // has none, which is what a tag every target already carries looks like.
    hint: aside ? aside.textContent : "",
  };
};
/** One table cell's entries.  The header's cells hold a word rather than
 * entries and read as one; an empty cell reads as nothing. */
const paletteCell = (cell) =>
  cell.children.length ? cell.children.map(paletteEntry)
    : cell.textContent ? [asWord(cell.textContent)] : [];
/**
 * The value palette's list as it stands: per ROW of `#plist', its class, the
 * source it names, and the entries in its Active and Inactive halves.  The
 * hairlines between rows are the rows' own borders, so what is observable here
 * is the table's shape rather than a divider.
 *
 * Three row shapes, told apart by what the producer put in them.  A table row
 * carries its two cells.  A row holding ONE entry is the meta's, or the
 * fallback mode's own body, and reads as that entry in the active half.  A row
 * holding neither is the standing line, and reads as its text.
 */
const paletteRows = () => field("plist").children.map((row) => {
  const cells = parts(row, "pc"), own = parts(row, "pe")[0] || row;
  const [active, inactive] = cells.length ? cells.map(paletteCell)
    : [[parts(own, "pw").length ? paletteEntry(own) : asWord(row.textContent)], []];
  return {
    cls: row.className,
    source: (parts(row, "ps")[0] || {}).textContent || "",
    active,
    inactive,
  };
});

/**
 * The log strip as it stands: a line's severity class and the text it renders,
 * the parts joined by the space that separates them on screen.  The repeat
 * counter is empty until a line repeats, which is why the empty parts go.
 */
const logged = () => field("log").children.map((line) => ({
  sev: line.className,
  text: line.children.map((part) => part.textContent).filter(Boolean).join(" "),
}));

// What `assign' worked out, as `LETTER@INDEX' per entry and `-' for one that
// claimed nothing.
let assigned = [];

// What `cells' worked out: the column indices an edit overlay's shape resolves
// to as `FROM,TO', `«none»' where one of its keys names no column in the list,
// and null where the act never ran.
let span = null;

/** A stored value as the answer spells it, `«unset»' for a key that is not
 * there — which is a different state from one holding the empty string. */
const unset = (v) => (v === null ? "«unset»" : v);

const ACTIONS = {
  close: (reason) => { if (socket && socket.onclose) socket.onclose({ reason }); },
  // The which-key assignment driven as the pure function it is: a comma-separated
  // cycle in, the claimed letters out.  The glue is eval'd into this scope, so
  // its own function is what answers — no second copy of the rule here.
  assign: (arg) => {
    const labels = arg.split(",");
    assigned = whichKeys(labels).map((at, i) =>
      (at === -1 ? "-" : `${letterAt(labels[i], at)}@${at}`));
  },
  // And the edit overlay's cell resolution, likewise driven as the pure function
  // it is: `KEYS@COLUMNS', both comma-separated, the column list being the KEYS
  // of the columns the server declared for that popup.  The glue's own function
  // answers — there is no second copy of the rule here.
  cells: (arg) => {
    const [keys, cols] = arg.split("@");
    const at = cellSpan(keys ? keys.split(",") : [],
                        (cols ? cols.split(",") : []).map((key) => ({ key })));
    span = at ? at.join(",") : "«none»";
  },
  // The resolution never arrives, which is what leaves the palette standing in
  // the state between the press that raised it and the answer that fills it.
  stall: () => { stalling = true; },
  // The resolution a marked set spanning two tags comes back as: org's own pair
  // and then two tag sources, in the order the server put them, and no file
  // layer at all.
  // The mixed set normalize-up is about: two of the three rows carry `web' and
  // the third does not, so the first press over the set is the levelling one.
  partly: () => { rowTags = { r1: ["web"], r2: ["web"], r3: [] }; },
  // Rows with no tags at all, which is where a first `:' has nothing to list
  // and `/' is the only way in.
  untagged: () => { rowTags = { r1: [], r2: [], r3: [] }; },
  // A store that knows none of the rows the palette named.
  unknownrows: () => { rowTags = {}; },
  twotags: () => {
    sources = [
      { source: "default", active: ["TODO"], inactive: ["DONE"] },
      { source: "book", active: ["READING"], inactive: ["READ"] },
      { source: "film", active: ["WATCHING"], inactive: ["WATCHED"] },
    ];
  },
  sheet: (text) => { field("mtext").value = text; },
  filter: (text) => { field("filter").value = text; },
  moved: () => {
    step();
    rows = rows.concat([{ id: "r4", cells: { state: "TODO", title: "four", tag: "" } }]);
    served += 1;
  },
  recolumn: () => { step(); columns = columns.concat([{ key: "deadline" }]); },
  // ROW FRAMES, delivered down the LIVE SOCKET the way the watcher delivers
  // them: `socket.onmessage' is the page's own door and this is the only way
  // in, so what a frame reaches is the shell's real handling of one.
  // `frame:upsert=r1' re-sends a row that moved — which is what an ARCHIVE puts
  // on the wire, the row still being the store's — and `frame:delete=r1,r2'
  // says two rows are gone, so the served set loses them with the frame.
  // An unfiltered client splices these straight in; a filtered one reads none
  // of them and refetches, which is what `unserved' is for.
  frame: (arg) => {
    const at = arg.indexOf("=");
    const op = at === -1 ? arg : arg.slice(0, at);
    if (op !== "upsert" && op !== "delete")
      throw new Error(`no such frame op: ${arg}`);
    if (!socket || !socket.onmessage)
      throw new Error(`no socket to carry a frame: frame:${arg}`);
    for (const id of (at === -1 ? "" : arg.slice(at + 1)).split(",").filter(Boolean)) {
      if (op === "delete") {
        // The frame FIRST, so an unfiltered client's own `deleteRow' is what
        // takes the row out and a shell that ignored the frame is visible in
        // what is left.  The store loses it either way: a `delete-row' IS the
        // store having lost it, and a filtered client never splices.
        socket.onmessage({ data: JSON.stringify({ op: "delete-row", id }) });
        rows = rows.filter((r) => r.id !== id);
        served -= 1;
        step();
        continue;
      }
      const row = rows.concat(hidden).find((r) => r.id === id);
      if (!row) throw new Error(`no such row to upsert: ${id}`);
      step();   // a frame is a store that moved, so the tag moves with it
      socket.onmessage({ data: JSON.stringify({ op: "upsert-row", row }) });
    }
  },
  // The applied query stops matching IDS: /headlines answers without them and
  // the tag steps, so a revalidation comes back with rows rather than a 304.
  // It describes an APPLIED QUERY, so pairing it with an unfiltered boot means
  // nothing — an unfiltered client splices a frame back in and undoes it.
  // The rows themselves stay the store's, which is what lets a frame still
  // carry one — an archive is an upsert on the wire and an absence in the
  // answer, and this is the second half.
  unserved: (arg) => {
    const ids = arg.split(",").filter(Boolean);
    hidden = hidden.concat(rows.filter((r) => ids.indexOf(r.id) !== -1));
    rows = rows.filter((r) => ids.indexOf(r.id) === -1);
    served -= ids.length;
    step();
  },
  rewritten: () => { digest = "d1"; },
  press: (key) => press(key),
  // A MOUSE CLICK landing on another row of a modal MOUNT, which is the ONE
  // thing that can move a cursor out from under an open edit overlay — no key
  // can, which is why every other act here is a key.  `click:2' is the reader
  // clicking row 2 of whichever popup is up: the link popup, then the tags one.
  // The structured document is not a mount and binds no click at all, so it has
  // no such hazard and no act for it.  The renderer moves its own cursor and
  // tells this page nothing, so what this measures is whether a commit still
  // writes the row the overlay OPENED over.
  click: (at) => {
    const m = field("modal").className === "on" ? pan
      : field("links").className === "on" ? lnk : tgs;
    if (!m) throw new Error(`no modal mount to click in: click:${at}`);
    const i = Number(at);
    if (!(i >= 0 && i < m.own.length))
      throw new Error(`no row ${at} to click in the mount`);
    m.sit(i);
  },
  // The settings sheet's theme select, driven the way a reader drives it: focus
  // it, pick a theme, and let the change event fire.  What it is here to show is
  // what happens AFTER — the theme applied, the choice stored, and the sheet
  // still standing over the table it was raised from.
  theme: (name) => {
    const box = field("themesel");
    box.focus();
    box.value = name;
    box.fire("change", { target: box });
  },
  // The same key delivered as an AUTO-REPEAT, which is what the ONCE list is
  // about: the dispatch claims it either way and runs it only when it is not
  // one of the commands a hold must not repeat.
  repeat: (key) => press(key, true),
  // A keydown with NO keyup and `repeat' UNSET: the lying auto-repeat the
  // native window's GTK layer produces.  Two of these in a row are one held
  // key however the event spells it.
  stuck: (key) => press(key, false, true),
  // The field is the fallback mode's and is hidden until `/' raises it, so a
  // script that types without pressing `/' first is typing into nothing on a
  // real page: say so rather than narrow a list no reader could have narrowed.
  type: (text) => {
    if (field("pbox").className !== "narrow")
      throw new Error("the value palette is not in its typing mode");
    const box = field("pinput");
    box.value = text;
    box.fire("input", { target: box });
  },
  // Typing into the tags popup's rename overlay, which is one field over the tag
  // at point.  A closed overlay has no field, so a script that types without
  // pressing RET first is typing into nothing on a real page: say so.
  tname: (text) => {
    if (field("tedit").className !== "on")
      throw new Error("no tag is open for renaming");
    typed(field("tname"), text);
  },
  // The pin button: a click on the strip's far edge, reaching whatever the
  // consumer wired.  A mount without the wire is a page without the button,
  // so pressing it is a script error rather than a silent nothing.
  pinclick: () => {
    if (!main.onPin) throw new Error("no onPin was wired: pinclick");
    main.onPin();
  },
  // Typing into the capture form.  The tag and the line are its own markup; a
  // template's grown fields are page-made nodes, so `kf:' types into whichever
  // of them holds the focus — the way a reader reaches one.
  ktag: (text) => {
    if (field("capture").className !== "on")
      throw new Error("the capture form is not open: ktag");
    const box = field("ktag");
    box.focus();
    typed(box, text);
  },
  kf: (text) => {
    if (field("capture").className !== "on")
      throw new Error("the capture form is not open: kf");
    if (!active || active === field("ktag") || active === field("ktext"))
      throw new Error("no template field holds the focus: kf");
    typed(active, text);
  },
  ktext: (text) => {
    if (field("capture").className !== "on")
      throw new Error("the capture form is not open: ktext");
    const box = field("ktext");
    box.focus();
    typed(box, text);
  },
  // And into the link popup's edit overlay, which is two fields over the link at
  // point: `ltitle' is what the entry calls it and `lurl' where it points.  A
  // closed overlay has neither, for the rename's reason.
  ltitle: (text) => typeLink("ltitle", text),
  lurl: (text) => typeLink("lurl", text),
  // Typing into the document's edit overlays: `dtin' is the one field a
  // headline's title opens as, and `dpara' is the textarea a paragraph opens
  // as.  Each is laid over the element at point, so no index is owed — no key
  // can move the cursor while one is open, and the document binds no click.
  dtin: (text) => typeIn("dtitle", "dtin", text),
  // ACTS SPLIT ON WHITESPACE, so a paragraph with a space or a line break in it
  // is spelled `_' and `|' here and cooked back on the way in.  A stop the walk
  // takes over several lines — a list item, a whole block — cannot be typed any
  // other way.
  // `_' is a space and `|' a newline, an act carrying neither; `~' is a
  // LITERAL bar, put back after the newlines so an org table row can be typed
  // into a paragraph that spells its own line breaks with the same character.
  dpara: (text) => typeIn("dpara", "dtext",
    String(text).replace(/_/g, " ").replace(/\|/g, "\n").replace(/~/g, "|")),
  pkey: (arg) => typeOver("pkey", arg),
  pval: (arg) => typeOver("pval", arg),
  // And into the settings sheet: `ctext:#+TODO:_A_|_B' is the keywords panel's
  // one box, holding the SELECTED layer's `#+TODO:' lines as the sheet edits
  // them.  Which layer that is comes off `clayer' below.
  ctext: (text) => (onKeywords(), typeSetting("ctext", text)),
  // TAKING AN EDIT BACK: the layer's own lines typed in again, which is what a
  // reader does and what the acts cannot spell — an act splits on spaces and a
  // `#+TODO:' line is mostly spaces.
  crevert: () => {
    onKeywords();
    const at = Number(field("clayer").value) || 0;
    const shown = layers.slice()
      .sort((a, b) => (a.tag === null ? 0 : 1) - (b.tag === null ? 0 : 1)
                   || String(a.tag).localeCompare(String(b.tag)))[at];
    typed(field("ctext"), (shown.lines || []).join("\n"));
  },
  // Picking a layer, the way a reader picks one: the select takes the focus, the
  // value moves, and the change event fires.  What it is here to show is that
  // the box under it swaps and an edit in the layer being left is still there on
  // the way back.
  clayer: (at) => {
    if (field("config").className !== "on")
      throw new Error("the settings sheet is not open: clayer");
    onKeywords();
    const box = field("clayer");
    box.focus();
    box.value = String(at);
    box.fire("change", { target: box });
  },
  // THE KEYWORDS PANEL, brought on screen where an act needs it there.  A box
  // is only typeable while its own tab shows — the page reads a box back only
  // then, since two editors write one cycle now — so an act naming that panel's
  // controls says so rather than every case spelling the tab press.
  ctab: (name) => {
    if (field("config").className !== "on")
      throw new Error("the settings sheet is not open: ctab");
    const tab = field("ctabs").children.find((t) => t.textContent === name);
    if (!tab) throw new Error(`no settings tab called ${name}`);
    tab.fire("click", {});
  },
  // Landing the states table's cursor on the row for one keyword.
  sat: (state) => {
    if (!sts) throw new Error("the states table is not mounted: sat");
    const row = sts.own.find((r) => r.cells.state === state);
    if (!row) throw new Error(`no state row for ${state}`);
    sts.handle.select(row.id);
  },
  // And what the open edit overlay's three fields hold: `sfields:NAME/GROUP/HUE'
  // — an empty part leaves that field as it was.
  sfields: (spec) => {
    const [name, group, hue] = String(spec).split("/");
    if (name !== undefined && name !== "") field("sname").value = name;
    if (group !== undefined && group !== "") field("sgroup").value = group;
    if (hue !== undefined && hue !== "") field("shue").value = hue;
  },
  // Picking WHICH saved view the composer stands on, the same way: the value
  // moves and the change fires, and what it shows is that the box swaps and an
  // edit in the view being left survives the trip back.
  cwhich: (id) => {
    if (field("config").className !== "on")
      throw new Error("the settings sheet is not open: cwhich");
    const box = field("cwhich");
    box.value = String(id);
    box.fire("change", { target: box });
  },
  // And the general panel's three fields, which are fixed rows rather than a
  // layer's: the default view and the capture target, bound to the system
  // layer and posted in its write, and the log knob, which is stored here and
  // posted nowhere.  They are markup rather than a drawn row, so typing into
  // them with the sheet shut would write where no reader could have.
  // The default-view composer: the act is an Enter commit — the held query
  // moves and the consumer's onFilter hears it.
  cview: (text) => {
    if (!cmp) throw new Error("the composer is not mounted: cview");
    cmp.held = text;
    if (cmp.onFilter) cmp.onFilter(text);
  },
  ccap: (text) => typeSetting("ctarget", text),
  clog: (text) => typeSetting("clog", text),
  // Every config layer moves out from under the sheet, which is the drift a
  // second writer causes.
  cmoved: () => { for (const l of layers) l.digest = "gone"; },
  // Per-row priority cells, comma-separated and positional: `priorities:A,,C'
  // gives row one `[#A]', row two none and row three `[#C]'.  A cell the store
  // does not hold is what an entry with no priority IS, which is the ring's own
  // `none' stop — so a MIXED set is one act.
  priorities: (arg) => {
    arg.split(",").forEach((p, i) => {
      if (!rows[i]) return;
      if (p) rows[i].cells.priority = `[#${p.toUpperCase()}]`;
      else delete rows[i].cells.priority;
      if (i === 0) headPriority = p ? `[#${p.toUpperCase()}]` : null;
    });
  },
  // The body the GRAIN walk is measured over — see `grainBody'.  Set before the
  // sheet opens, since the document is built out of the answer.
  grain: () => { grainy = true; },
  // And the body the CHECKBOX toggle is measured over — see `checkyBody'.
  checky: () => { checky = true; },
  // And the body the TABLE grain is measured over — see `tabledBody'.  Set
  // before the sheet opens, since the document is built out of the answer.
  tabled: () => { tabled = true; },
  // The body every link shape is in, and the scan that goes with it.
  linky: () => { linky = true; links = linkyLinks; },
  // Two links in the grainy body: one inside the list's FIRST item and one
  // inside its second.  Which is what makes `o' at a leaf and `o' at the whole
  // list two different questions — the same answer, narrowed by the stop's own
  // extent.
  grainlinks: () => {
    links = [ { target: "https://alpha.example/", desc: "in alpha",
                type: "https", span: [21, 40] },
              { target: "https://beta.example/", desc: "in beta",
                type: "https", span: [53, 58] } ];
  },
  refuse: () => { refusing = true; },
  // Nothing refers to the row `@' names, which is the answer that leaves the
  // table standing rather than replacing it with an empty view.
  noreferences: () => { unreferenced = true; },
  // A click on an open sheet's own chrome — its header, its file line — takes
  // the focus off whatever field had it without closing anything.  That is when
  // `typing()' goes false again and every `table' row comes back to life over a
  // sheet that is still up, which no other act reaches.
  blur: () => { if (active) active.blur(); },
  // An asset that never had marking: the calls are simply not on the handle,
  // which is the shape the shell's feature detection is written against. It
  // sticks, so a remount later in the same script does not hand them back and
  // quietly turn the fallback case into the ordinary one.
  bare: () => { markless = true; stripLive(MARK_CALLS); },
  // And one that never had paging, which is what leaves the buffer-end keys
  // their within-page half and nothing to climb with.
  pageless: () => { pagerless = true; stripLive(PAGE_CALLS); },
  // And one with no programmatic sort, which is what leaves the agenda with
  // the order the view declares and nothing to insist on it.
  sortless: () => { sortnone = true; stripLive(SORT_CALLS); },
  // And one with no crumb trail, which is what leaves `@' nowhere to leave a
  // step behind: the drill is refused outright rather than applying a view a
  // reader would have no way back out of.
  crumbless: () => { crumbless = true; stripLive(CRUMB_CALLS); },
  // What the row `o' names points at: one link, or none at all.  The gesture
  // is different for each — one opens without asking, none refuses — and the
  // three-link default is what raises the popup.
  onelink: () => { links = links.slice(0, 1); },
  nolinks: () => { links = []; },
  // One link that is not http(s): `o' takes the no-popup path straight to the
  // commit, which is where a link type this page cannot follow is refused.
  onemailto: () => { links = links.slice(2, 3); },
  // Every type the server derives, one link each, so the popup's badge column
  // and the commit's judgement can both be read over the whole vocabulary.
  everytype: () => {
    links = [ { target: "https://a.example", desc: "secure", type: "https" },
              { target: "http://b.example", desc: "plain", type: "http" },
              { target: "org-glance-visit:XYZ", desc: "the other row", type: "glance" },
              { target: "mailto:t@example.org", desc: "write", type: "mailto" },
              { target: "id:99", desc: "org's own", type: "id" },
              { target: "file:notes.org", desc: "a file", type: "file" },
              { target: "Some Headline", desc: "Some Headline", type: "other" } ];
  },
  // A store with pages in it: N rows in place of the three at the top, and the
  // renderer showing SIZE of them at a time.  Acts rather than argv, so every
  // script that wants neither reads exactly as it did.
  rows: (n) => {
    rows = Array.from({ length: Number(n) }, (_x, i) =>
      ({ id: `r${i + 1}`, cells: { state: "TODO", title: `row ${i + 1}`, tag: ":web:" } }));
    main.pageAt = 0;
    main.sit(0);
  },
  paged: (n) => { main.pageSize = Number(n); main.pageAt = 0; main.sit(0); },
  // N distinct lines through the page's own `append': the glue is eval'd into
  // this scope, so its functions are reachable from here.  The ring holds five
  // hundred and nothing a key presses writes them faster than one at a time, so
  // a script that overran it any other way would be longer than the cap.
  spam: (n) => {
    for (let i = 0; i < Number(n); i += 1) append("boot", "info", `line ${i}`);
  },
  // The daemon goes away: every request after this fails at the network, which
  // is what the reconnect's error line and the retry behind it are written for.
  offline: () => { down = true; },
  // And comes back, which is what the retry behind the backoff finds.
  online: () => { down = false; },
  // /headlines stops answering, so a view application can be observed WHILE it
  // is in flight — the state the wash is armed by and the one turn of the loop
  // every other answer here is already past.
  hang: () => { hanging = true; },
  // And answers: every request held since, in the order they were made.
  deliver: () => {
    hanging = false;
    while (held.length) held.shift()();
  },
  // The same pair over the settings sheet's own write: `C-x C-s' syncs mid-edit,
  // so a reader can go on typing while it is out, and what lands afterwards must
  // not paint over what they typed.
  chang: () => { changing = true; },
  cdeliver: () => {
    changing = false;
    while (cheld.length) cheld.shift()();
  },
  // Time passing, which is the one thing a delayed state needs and no other act
  // can stand in for: the wash arms on a timer and a script has to be able to
  // sit either side of it.
  wait: (ms) => new Promise((done) => setTimeout(done, Number(ms))),
};

// Every fetch here settles as a microtask, so one turn of the event loop is
// past the whole boot — the arming fetch chained behind the set included.  The
// keys go in after that, then the acts one at a time, and the answer last: a
// close leads to a fetch which leads to a mount, and each of those needs its
// own turn before the next act can mean anything.
const settle = () => new Promise((done) => setTimeout(done, 20));
(async () => {
  await settle();
  for (const key of (keys || "").split(/\s+/).filter(Boolean)) press(key);
  await settle();
  for (const act of (acts || "").split(/\s+/).filter(Boolean)) {
    const at = act.indexOf(":");
    const verb = at === -1 ? act : act.slice(0, at);
    if (!ACTIONS[verb]) throw new Error(`no such act: ${act}`);
    // Awaited, so an act that takes time — `wait' is the only one — is over
    // before the next reads the page.
    await ACTIONS[verb](at === -1 ? "" : act.slice(at + 1));
    await settle();
  }
  await settle();
  const said = JSON.stringify({
    asked, tags, url: location.search, mounts, sets, raises,
    // The stale wash: every transition of the class, oldest first, and whether
    // it is on at the end.  A page that was never dimmed reports neither.
    washed, stale: root.classList.contains("stale"),
    // And every row count the table was handed, which is what says whether a
    // view arrived in one piece, plus every row op it spliced in without one.
    paints, spliced,
    sheet: field("mtext").value, state: field("mnote").className,
    modal: field("modal").className,
    palette: field("filter").value,
    // THE STRUCTURED DOCUMENT: every element it drew, where the cursor is and
    // which cell of that element it is in, which elements wear a deletion flag,
    // whether an edit overlay is open and what its fields hold, and the
    // breadcrumb saying where in the outline the sheet is standing.
    doc: docRows(), dat: docAt(), dcol: docCell(), dflagged: docFlagged(),
    dopen: field("dtitle").className === "on",
    dparaopen: field("dpara").className === "on",
    dtin: field("dtin").value,
    dtext: field("dtext").value,
    // The sheet's crumb strip: one entry per step of the descent, the LAST
    // wearing the full-ink class that says where the reader stands.  Read as
    // the parts it drew rather than as one string, since the bar is a row of
    // chips and the ink is what tells the standing one from the trail.
    where: field("mwhere").children.map((c) => c.textContent),
    whereAt: field("mwhere").children
      .map((c, i) => (wears(c, "wat") ? i : -1)).filter((i) => i !== -1),
    // Which pane holds the keys: the document until TAB crosses, the panel
    // after it, and each says so on its own frame.
    dactive: field("mdoc").className === "on",
    // The column CONTENT lines start at, written onto the pane as a number the
    // stylesheet does the arithmetic over — the width of the head's own star
    // prefix, so the content sits under the title text rather than under the
    // stars (`org-startup-indented').
    dindent: field("mdoc").style.getPropertyValue("--g-doc-indent"),
    // Every element the page asked to be scrolled to, by class, and what the
    // last one asked for.  Geometry is beyond the stub, so the CALL is the
    // whole of what these pin.
    // Which STOP KIND each element of the walk is: `element' for a plain
    // paragraph and for the headline and child lines, `composite' for a whole
    // list or block, `leaf' for one item or one paragraph inside one.
    dgrains: flatRows().map((row) => (wears(row, "d-comp") ? "composite"
      : wears(row, "d-item") ? "leaf" : "element")),
    // And who each leaf hangs under, by place in the walk — `-1' for a stop
    // that is nobody's leaf.
    downers: ownerOf(),
    // What the document drew as links, and how each element was cut up.
    dsegs: flatRows().map(segsOf),
    // The head row's title cell's OWN text node.  A browser shows textContent
    // and appended children side by side, so a cell that drew segments must
    // hold no raw text of its own — the double-draw this field exists to see.
    dtitleraw: (() => {
      const head = flatRows()[0];
      const cell = head && head.children.find((c) => wears(c, "dc-title"));
      return cell ? String(cell.textContent || "") : "";
    })(),
    scrolled: scrolls.map((s) => s.className),
    scrollAsked: scrolls.length ? scrolls[scrolls.length - 1].opts : null,
    // The sheet's other pane: every row the panel is showing, where its cursor
    // is, whether it is the thing holding the keys, and which of its rows carry
    // a delete flag — plus the mount options the gesture reads.
    props: panel(), pat: patAt(), pnav: field("mprops").className === "on",
    pmounts, psets, ...mountFields("p", pan),
    focus: focused(),
    // Every POST the syncs sent, and which SUBTREE each was aimed at — the row,
    // or an entry inside it — beside every subtree a GET asked for.
    wroteAt, readAt,
    // What holds the keyboard, as its tag — empty for nothing, which is the
    // state the table's own keys are live in.
    holding: active ? active.tagName : "",
    // The logbook strip: shown, never focusable, never written.
    logbook: field("mlog").textContent,
    shape: field("sheet").className, writes,
    // The renderer's side of marking, and the last thing the echo pill said —
    // which is where a key that could not do what it was asked reports it.
    marksOn: main.marksOn, hintsOn: main.hintsOn, flagHelp: main.flagHelp,
    marked: [...main.marks], flagged: [...main.flags], cursor: main.at(),
    // Where the cursor is in terms a page-local index cannot give: the row it
    // sits on, the column it is in, and the page it is reading.  A table
    // nothing has selected in reports -1 and a null row, which is a boot that
    // landed nothing.
    selected: main.at() === -1 ? null : main.onPage()[main.at()].id,
    col: main.selCol,
    page: main.pageAt + 1,
    echo: field("echo").textContent, echoes: field("echo").wrote,
    // The event strip, which is append-only: what is here is everything the
    // page has said since it booted, oldest first.
    log: logged(),
    // The value palette: whether it is up, which mode it is in, what it is
    // setting, the resolution it drew, which rows it resolved for, the keys it
    // names, and what a commit posted.
    prompt: field("prompt").className, phead: field("phead").textContent,
    pmode: field("pbox").className, plist: paletteRows(), resolved,
    pfoot: field("pfoot").textContent, assigned, commands, span,
    // Following a link: which rows were asked about, which tabs were opened,
    // the last sort a call asked for and how many were asked for, and the CHAIN
    // in force — which the query names and no call has to have made.
    linked, opened, sorted, sortCalls, chain: sortChain, tagged,
    // The pin badge, as the consumer last set it.
    pinned: main.pinned,
    // The capture form: whether it is up, its head, the tag field, the grown
    // template fields as [label, value] pairs, and the line.
    capture: field("capture").className, khead: field("khead").textContent,
    ktag: field("ktag").value, ktext: field("ktext").value,
    kfields: field("kfields").children.map((row) => [
      (row.children[0] || {}).textContent || "",
      (row.children[1] || {}).value || "" ]),
    // The link popup, which is the page's THIRD mount: whether it is up, the two
    // lines of chrome it draws, how many times it was built and re-set, the rows
    // it is showing, where its cursor is, and the read-only options it was
    // mounted with — no marks, no flags, no hint line, no page.
    popup: field("links").className, lhead: field("lhead").textContent,
    lfoot: field("lfoot").textContent, lmounts,
    llinks: cellsOf(lnk, ["type", "title", "url"]), lat: curOf(lnk),
    ...mountFields("l", lnk),
    // The link popup's edit overlay: whether a link is open for editing and what
    // its two fields are holding.
    lopen: field("ledit").className === "on",
    ltitle: field("ltitle").value, lurl: field("lurl").value,
    // The tags popup, which is the page's FOURTH mount and the one that WRITES:
    // whether it is up, its two lines of chrome, how many times it was built and
    // re-set, the rows it shows with their coverage and their store-wide counts,
    // where its cursor is, which tags wear a removal flag, and the options it
    // was mounted with — no marks, flags on, no hint line, no page.
    tagpop: field("tags").className, thead: field("thead").textContent,
    tfoot: field("tfoot").textContent, tmounts, tsets,
    // Spelled, since the count cell is a number and the other two are words:
    // one shape for a reader to assert against.
    ttags: cellsOf(tgs, ["title", "on", "rows"]).map((cells) => cells.map(String)),
    tat: curOf(tgs),
    ...mountFields("t", tgs),
    // The rename overlay: whether a tag is open for editing and what its one
    // field is holding.
    trename: field("tedit").className === "on", tname: field("tname").value,
    // The drill-down trail as the strip would draw it, labels alone — the
    // queries behind them are the shell's business and the URL already carries
    // them.
    crumbs: main.crumbs.map((c) => c.label),
    // Which keys the dispatch took off the browser, in press order.
    prevented,
    // The settings sheet: whether it is up, the one word it wears, the union it
    // previews, and every write it sent.
    settings: field("config").className, cstate: field("cnote").className,
    // The keywords panel: the layers the select offers in the order it offers
    // them, which one is picked, the lines the one box is showing, the label
    // over it, and whatever the server last said about a write to that layer.
    // A layer's OTHER text is in memory and off screen, which is what switching
    // back shows.
    clayers: field("clayer").children.map((o) => o.textContent),
    cat: field("clayer").value, cshown: field("ctext").value,
    clab: field("clab").textContent, clerr: field("clerr").textContent,
    // The panels, by the header each wears, in the order the sheet draws them.
    // The tabs, and which one is showing: one panel at a time now.
    csecs: field("ctabs").children.map((t) => t.textContent),
    ctab: (field("ctabs").children.find((t) => t.className === "ctab on")
             || { textContent: "" }).textContent,
    // What the two tree-wide fields are showing, and what the server holds now.
    cview: cmp ? cmp.held : "", cmounts, ccap: field("ctarget").value,
    served: viewQuery, servedAgenda: agendaQuery,
    servedCapture: captureLine, capturing: captureAsked,
    // Which saved view the composer is standing on.
    cwhich: field("cwhich").value,
    // The states table: one `TAG|STATE|GROUP|COLOUR' per row, in the order the
    // mount holds them, plus how many times it was mounted and where its
    // cursor is.
    chues: cellsOf(sts, ["tag", "state", "group", "colour"]).map((c) => c.join("|")),
    smounts, sat: curOf(sts),
    // The states table's edit overlay, and what its three fields hold.
    sedit: field("sedit").className,
    sfields: [field("sname").value, field("sgroup").value, field("shue").value],
    servedHues: stateHues,
    ctpl: field("ctpl").value,
    ceff: field("ceff").textContent, configWrites,
    // The log knob: what the field holds, what was stored under it, and the
    // number the page wrote onto the strip — which is the cap taking effect.
    // A key that is NOT THERE reads as the sentinel rather than as "": emptying
    // the field removes the preference, and "no preference" and "a preference
    // spelling the empty string" are the two states that has to be told apart.
    clog: field("clog").value, logStored: unset(localStorage.getItem("glance-log")),
    logn: field("log").style.getPropertyValue("--g-logn"),
    // The theme panel: what is stamped on the document element and what was
    // stored under it — `auto' is the attribute coming OFF, so it reads as "".
    theme: root.dataset.theme || "",
    themeStored: localStorage.getItem("glance-theme"),
  });
  // Exit on the write's own callback: a keystroke leaves the echo pill's timer
  // pending, and node would otherwise sit out its second and a half.
  process.stdout.write(said + "\n", () => process.exit(0));
})();
