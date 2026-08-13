# Proposal — org-console-web

**Status:** draft — draft, rev 2 — Emacs-free core; deterministic automation · **Date:** 2026-07-30

Glance grows from a batch org parser into the **backend of a browser-rendered,
browser-driven org runtime**: org files stay canonical, glance reads them,
serves views, and applies structured write-back; a browser renders the views
and acts as an automation surface driven by deterministic scripts. Emacs is one
client among peers. Working name: **org-console-web** — a shift from a pure
console to console+web org interaction.

## Problem & drivers

The org store is rich but locked inside Emacs on one machine, rendered as text.
The wanted payoffs, in priority order:

1. **Access from other devices** — read/navigate the graph without an Emacs there.
2. **Better graph/mindmap visualization** — browsers do interactive graphs; Emacs does not.
3. **Share views** — publish a read-only slice to people who do not run Emacs.
4. **Automation** — drive web workflows from org, using a real logged-in browser
   session (cookies), driven by deterministic scripts.
5. **Workspace** — edit files (code, raw org) and run a terminal from the
   browser tab, over the same daemon and bridge. Materialize a headline's
   subtree, change it, commit it back drift-locked.

**Offline, clarified (rev 3).** Same-device offline already holds: the daemon
reads local files and the browser talks to 127.0.0.1 — no network anywhere in
the loop. Cross-device offline (a phone with no reachable daemon) is served by
the file-sync layer already in place (`~/sync`) plus a daemon per device over
its own replica; write-back's optimistic lock plus sync-level conflict handling
cover concurrent edits. Browser-without-daemon stays a non-goal — WASM remains
rejected (see Non-goals).

## Non-goals & rejected approaches

- **Emacs compiled to WASM** — tens of MB, slow boot, no subprocesses, painful
  IO, mobile-hostile. Boils the ocean to ship one workflow.
- **Reimplementing elisp / org in the browser** — years of work; reimplements
  the editor. Scope note (rev 3): this bars *semantic* org editing in the
  browser (refile, agenda logic, folding cycles). Embedding an off-the-shelf
  text component (CodeMirror) for raw text is a component choice, covered by
  the write-back rules below.
- **Building our own browser (Nyxt-style)** — Nyxt's *browser* is the weak part
  (WebKit jank, unpolished UX). Its *extension model* is the good part. Keep the
  model, use a real browser. Rev 4 nuance: a **desktop shell** — a native
  window hosting the *system* webview pointed at the local daemon — is not an
  own browser (we never own the engine) and is now planned (see Desktop
  shell). The automation driver stays in the user's real browser regardless:
  a webview shell has no logged-in sessions, cookies live in the real browser.
- **WASM in general** — only offline needs it, and offline is not a driver.
  Rev 4, performance verdict: measured, not needed. The 13k-row filter freeze
  was DOM churn (874 ms of innerHTML rebuild + 33k listener re-attachment per
  keystroke); after renderer surgery the same keystroke is 19 ms of work and
  the server filter is 7–10 ms. WASM cannot touch the DOM and would add
  marshalling on top of a compute cost that is already noise. Standing
  verdict: WASM buys nothing here.
- **LLM in the automation loop** — a model steering a cookie-bearing browser
  session is a prompt-injection and session-theft surface. Automation runs as
  deterministic scripts: auditable, replayable, reviewed before they ever hold
  the session. LLM tooling may author or propose scripts offline; the
  privileged path executes only reviewed deterministic code.

## Architecture

Two runtimes joined by a bridge:

- **glance = brain.** Owns the read-model. Reads canonical org files (re-scans a
  directory, the way `repos` re-scans git), serves view-JSON, hosts the bridge.
- **browser = eyes + hands.** Renders the views *and* acts as a live automation
  surface with the user's session/cookies (via a browser extension).
- **bridge = the one primitive.** An HTTP/WS protocol both sides speak; it also
  doubles as the extension API.

Clients — Emacs, the browser extension, a phone, polyglot extensions — are all
peers of the daemon. **Emacs-free core (decided):** the daemon owns the full
loop — read, serve, structured write-back, orchestration. Emacs keeps zero
special role: one editor and one renderer (`table-view.el`) among peers;
nothing in the serving or writing path requires it. A dedicated daemon owns the
live bridge (decided against reusing the Emacs daemon).

**Drift avoidance:** org files remain the single source of truth; the daemon
keeps no second authoritative store, so read-model and files cannot diverge.
Files do have two kinds of writers — editors (freeform text) and the daemon
(structured surgical edits) — so daemon writes are guarded: optimistic lock
(hash/mtime vs parse snapshot, reject on drift) and atomic replace (temp +
rename); Emacs picks up daemon writes via `auto-revert-mode`.

## Key decisions

- **Emacs-free core.** The daemon serves *and* writes. Write-back = surgical
  span edits: the parser retains source offsets (megaparsec `getOffset`) for
  the mutable parts of a headline — TODO keyword, tags, priority, timestamps,
  drawers — and commands replace spans, leaving untouched bytes byte-identical.
  Writes are structured commands only (toggle state, retag, reschedule, append
  note, capture); freeform editing stays in editors. Org semantics that used to
  live in Emacs config (keyword sets, tag inheritance, archive targets) live
  in-file (`#+TODO` already parsed) plus a glance config file (`Data.Config`).
- **Orchestration: daemon-centric, deterministic.** Automation logic is
  deterministic scripts running as protocol clients; the LLM stays out of the
  loop (see Non-goals). This settles brain location: the daemon owns
  orchestration, Emacs is a client.
- **Backend language: Haskell (glance).** The parser, data model, graph intent
  (`fgl`, `RefKind`), and persistence scaffold already exist here.
- **Extensibility: protocol-first, not language-bound.** Extensions are processes
  that speak the bridge protocol (LSP/DAP model), so they can be written in any
  language. The AOT core does not gate them. "Live" happens via `ghcid` for core
  dev and hot-swappable extension processes for users; the `Repl.Org` REPL grows
  into a command surface.
- **Shared view contract: the table-view JSON schema.** Already emitted by
  `repos` (Haskell) and `ray-cluster` (Python) and rendered by `table-view.el`.
  glance becomes another producer of the same contract.
- **A sibling graph contract** (`RefKind` edges → `fgl` → graph-JSON) feeds a
  cytoscape/d3 renderer for the mindmap.

## The convergence

A parsed headline is a table-view **row** (TODO/DONE → state badge, tags,
priority, scheduled date → cells). So glance is *just another table-view
producer*, alongside `repos` and `ray-cluster`. **One renderer serves all
three.** The mindmap is a second, sibling contract from the same read-model.

*(Narrowed 2026-08-01: a row is a LEVEL-ONE headline. Deeper ones are carried
inside their entry's subtree and reached by materializing it —
`docs/invariants.md`, "A row is a top entry".)*

One contract, many producers (repos / ray / glance), many clients (Emacs /
browser / extensions).

## Renderer placement — `table-view.js`

`table-view.js` lives **inside `~/sync/stuff/table-view`**, beside
`table-view.el` — not as an isolated repo. The repo's asset is the *contract*;
`.el` and `.js` are two renderers of it. Splitting them fragments the schema and
invites drift.

```
table-view/
  table-view.el          ; Emacs renderer (exists)
  table-view-test.el
  examples/              ; elisp examples (exist)
  web/
    table-view.js        ; browser renderer (new)
    demo.html
  SCHEMA.md              ; the contract, language-agnostic (new, canonical)
  fixtures/              ; shared JSON specs both renderers test against (new)
```

Promote the contract — today documented only in the `table-view.el` commentary —
into a canonical `SCHEMA.md` that glance/repos/ray emit and el/js consume.

## Extensibility model

Steal Nyxt's model, drop its browser:

- **Protocol as the extension API.** Extensions connect over WS, register
  capabilities, receive/emit messages (LSP-style). Polyglot by construction.
- **Commands as data.** `table-view` already models actions as data
  (`(command . "pull")` dispatched to a handler); generalize that shape into the
  protocol's command layer.
- **Optional in-process fast-path** (e.g. an embedded scripting language) only if
  protocol processes prove too coarse for hot hooks. Baseline needs it not.

## Milestone spine

Step-by-step plan with exit criteria: [plan-org-console-web.md](plan-org-console-web.md).

Each milestone ships and is useful alone. The table needs only the finished
parser; the graph needs refs — sequence accordingly.

- **M0 (days).** Add `aeson` + a web server (`scotty`/`warp`). `GET /headlines?dir=…`
  → parse → rows as table-view JSON. Serve `table-view.js`. **Org headlines in a
  browser tab.** No refs, no DB.
- **M1.** WS + file-watch → live row upsert (repos/ray already do this with table-view).
- **M2.** Wire `RefKind` → `fgl` → `GET /graph` → graph-JSON → cytoscape. **The mindmap.**
- **M3.** Formalize the WS protocol as the extension API (capability registration,
  events) with privilege tiers in the first spec: public read-only share, write
  commands, and automation are separate capabilities behind auth. Browser
  extension = hands (cookies/automation); deterministic scripts = brain; both
  are protocol clients.
- **M4.** Commands round-trip; the daemon writes back to canonical org via span
  edits. Capture lands here too — phone capture turns driver 1 read-write.
- **M3.5 (workspace, after M3 tiers).** Raw-text editor pane (CodeMirror;
  replace-file for code, replace-subtree already serves org) and the browser
  terminal: xterm.js over the bridge WS to a daemon PTY. The terminal is the
  most privileged capability the bridge will ever carry — automate tier,
  never earlier than the tier system itself.

**UI principle (rev 4): keyboard-first.** Every surface is operable without
the mouse; keys mirror the Emacs org-glance maps (with a vim movement
profile); the key echo teaches the map. Buttons exist only where a keyboard
path cannot (never as the primary path): the materialize sheet is buttonless
(sync-on-close, `C-x C-s`), actions render as key hints in the hint line,
conflicts resolve by keystroke. Mouse still works; it is never required.

**Desktop shell (rev 4), both stages landed.** glance as a desktop
application, architecture unchanged (daemon + bridge + web UI; the shell is one
more client):

1. *Landed, zero code:* `glance desktop` — start the daemon, open an app-mode
   window (`chromium --app=http://127.0.0.1:PORT` / equivalent; fallback
   `xdg-open`). No browser chrome, dock icon, feels native.
2. *Landed behind `-f native-window`:* a WebKitGTK window in the binary. Stage
   1 chafed on the keyboard: Chromium and Firefox take `Ctrl+T`, `Ctrl+N` and
   `Ctrl+W` in the browser process above the document, so `C-c C-t` cannot
   complete in a borrowed window and the keymap has to carry a plain `t` as the
   spelling that works. A bare `WebKitWebView` in a plain `GtkWindow` has no
   chrome to bind those to. Electron stays rejected (a bundled Chromium per app
   contradicts the tiny-frontend rule); this borrows the system engine.

   Two modules: `Glance.Desktop.WebKit` in the private sublibrary
   `glance-desktop-native`, which is the engine and knows no daemon, and
   `Glance.Desktop.Native` in `glance-web`, which is the flow — prefer the
   native window unless a browser is named, fork the daemon, hand the main
   thread to GTK, stop the daemon when the window closes (`--keep-serving` to
   keep stage 1's semantics). The flow takes the window as a `String -> IO ()`,
   so it compiles and is tested in both flag states and the suite needs no GTK.

   The flag is manual and default-off, because on it the solver pulls ~28
   packages generated from the machine's typelibs. Every Hackage `gi-webkit2`
   binds WebKit2 **4.0** — the libsoup2 generation — which Arch has dropped,
   so `vendored/` carries it and `gi-javascriptcore4` repointed at the 4.1
   typelibs, and `cabal.project.native` is where the flag and those packages
   live together. Off, none of it resolves; on, `make native` builds it. What
   the flag still buys is that neither the suite nor CI ever pays for GTK.

The phone, the shared read-only slice, and the automation extension keep
using real browsers; the desktop shell adds a surface, replacing none.

**Materialize (landed early, ahead of M3/M4).** The subtree round-trip —
click a row, see the raw subtree, edit, commit back drift-locked — runs on
the S8 engine over localhost. It is the first raw-replacement command and the
proof of the write path; tier gating arrives with M3 before any non-localhost
exposure.

## Risks & mitigations

- **Mid-refactor churn.** The parser is split across two module trees
  (`Data.Org` monolith vs `Base/Context/Timestamp`). Do not let the web layer
  depend on parser internals. Add a thin stable **`Glance.Query`** facade
  (parse-dir → headlines → rows/graph); the daemon depends on that; the refactor
  stays behind it.
- **Wire format.** Add a dedicated `ToJSON` layer. `Display`/`TextShow` are REPL
  representations; keep them out of the browser contract.
- **Encrypted content (`aes`).** The elisp-`aes` format is the heaviest Emacs
  dependency left. Options: implement compatible decrypt in Haskell
  (`cryptohash`/`memory` already in deps; key-derivation compat is the fiddly
  part) or migrate subtrees to a standard format (age/gpg). Separately, decide
  per-view: decrypt client-side (key handling) or serve already-decrypted
  (trust the server). Affects the share driver.
- **Write-back.** All writes flow through the daemon's span-edit engine
  (drift-locked, atomic; landed early as the S8 core). The browser gets two
  kinds of write, both commands over the bridge: *structured* commands
  (toggle state, retag, reschedule, capture) and *raw replacement* commands —
  replace-subtree (materialize a headline, edit its raw text, commit back)
  and replace-file (code files in a CodeMirror pane). Raw replacement is
  whole-span text under the same drift lock; the daemon re-parses via the
  watch, so a saved file goes live everywhere. Semantic org editing stays out
  of the browser (see Non-goals). Rev 3 supersedes the earlier blanket
  "structured commands only" stance.
- **Prerequisites gate milestones.** M2's mindmap needs `RefKind` wired into
  parsing (currently defined, not wired) and enough graph assembly. Do not block
  M0 on refs or DB CRUD.

## Crux artifact — the protocol

Design **one JSON/WS protocol** that serves view-JSON (table + graph), takes
commands, streams events (file changed, browser event, automation result), and
*is* the extension API (register / notify). Emacs client, browser extension,
`table-view.js`, and polyglot extensions all speak it. The spec carries
privilege tiers from day one — read, write, automate — because the daemon
listens beyond localhost (drivers 1 and 3). Once specified, the rest is
plumbing.

Tier sketch (rev 3, to be specified at M3): **read** — views, graph,
materialize GET; **write** — structured commands and drift-locked raw
replacement (subtree, file); **automate** — terminal PTY and the
cookie-bearing browser extension. One token per tier, presented at
handshake; the daemon binds 127.0.0.1 until tokens exist, and the automate
tier additionally requires per-capability enablement at daemon start.

## Open questions

- **Browser attach** — extension (only place with the session/cookies) vs CDP
  attach. Automation driver needs the session, which points at an extension.
- **Graph renderer** — cytoscape vs d3 vs sigma for the mindmap.
- **Persistence** — when does the SQLite (`persistent`) layer earn its place?
  Parse-on-request suffices through M1; incremental indexing motivates it at scale.
