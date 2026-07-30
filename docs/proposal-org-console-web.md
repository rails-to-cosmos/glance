# Proposal — org-console-web

**Status:** draft, rev 2 — Emacs-free core; deterministic automation · **Date:** 2026-07-30

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

**Offline / no-server was explicitly *not* a driver.** That removes the only
reason to pursue WASM (see Non-goals).

## Non-goals & rejected approaches

- **Emacs compiled to WASM** — tens of MB, slow boot, no subprocesses, painful
  IO, mobile-hostile. Boils the ocean to ship one workflow.
- **Reimplementing elisp / org in the browser** — years of work; reimplements the editor.
- **Building our own browser (Nyxt-style)** — Nyxt's *browser* is the weak part
  (WebKit jank, unpolished UX). Its *extension model* is the good part. Keep the
  model, use a real browser.
- **WASM in general** — only offline needs it, and offline is not a driver.
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
- **Write-back.** Glance = navigate/browse first; browser stays read-mostly
  until M4, and all writes flow through the daemon. The early enabler is span
  retention: thread source offsets through the AST now, while the parser is
  ~280 lines — retrofitting offsets later is the expensive path. Freeform
  editing stays out of the browser permanently; structured commands only.
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

## Open questions

- **Browser attach** — extension (only place with the session/cookies) vs CDP
  attach. Automation driver needs the session, which points at an extension.
- **Graph renderer** — cytoscape vs d3 vs sigma for the mindmap.
- **Persistence** — when does the SQLite (`persistent`) layer earn its place?
  Parse-on-request suffices through M1; incremental indexing motivates it at scale.
