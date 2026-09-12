# Proposal — the commands are already MCP tools

**Status:** proposed · **Date:** 2026-09-08 · **Origin:** user — *"especially
for agents. So perhaps it would be good to have 'glance mcp' cli and api route,
what do you think?"*

## Where the wall is

An agent that wants to read or change a headline today has one door: HTTP by
hand. Read with `GET /headline?id=…` / `GET /headlines`; write with `POST
/command` carrying a verb and a JSON arg blob (`Routes.hs:139-145`). Works, but
the agent must know the port, hand-shape each request, and discover the verb set
and its args by reading source. There is no CLI CRUD at all — `app/Main.hs`
subcommands are `repl`, `serve`, `desktop`, `doctor`, `backfill-created`, none
of which touch headlines.

MCP (Model Context Protocol) is the protocol agents already speak for local
tools: a server advertises a typed tool catalog (`tools/list`) and runs calls
(`tools/call`). glance's command engine **is** that catalog under another name —
each `CommandSpec` in the `commands` table (`Commands.hs:137`) is a named
operation with typed args (`Args`, `Commands.hs:57`) and a dispatcher
(`runCommand` → `overRows`). Exposing MCP is a re-projection of what exists, not
a new engine.

## The law it must not break

Org files are the single source of truth; the store, index, and WAL are
projections rebuildable from text (README, "Under it"). MCP is one more door
onto the **same** write path — `runCommand` → `replaceSpans` (drift lock,
optimistic version, temp-then-rename) + the `EXTERNAL.jsonl` note. It adds a
transport, never a second writer of files.

## Two transports, one core

A single module — call it `Glance.Mcp` — owns the JSON-RPC framing
(`initialize`, `tools/list`, `tools/call`), the tool catalog derived from
`commands`, and dispatch. It takes a **store-runner** (something that resolves
`storeRecords` and applies a `Command`), so the same catalog serves both wirings:

| Transport | Wiring | Best when |
|---|---|---|
| `POST /mcp` route | the live Hub/Store of a running daemon | a daemon is up — live freshness, no re-walk, one process |
| `glance mcp --dir DIR` (stdio JSON-RPC) | a headless store booted offline, the `backfill-created` pattern (`Backfill.hs:234`) | the agent spawns glance directly (`.mcp.json`), no daemon needed |

stdio is the agent-native shape: an agent's `.mcp.json` launches a command and
speaks JSON-RPC over stdin/stdout, so `glance mcp` makes glance a drop-in agent
tool with zero HTTP config. The `/mcp` route is the multiplexed-daemon shape,
for an agent that already has the daemon's URL.

## The tool catalog, derived not hand-written

`tools/list` folds the `commands` table plus a few read verbs. Each write tool's
`inputSchema` comes from the `Args` fields the verb actually reads — so a new
verb in `commands` is a new MCP tool with no second edit, the same way it is a
new HTTP verb today.

Write tools (from `commands`): `capture` (create), `set-title`, `set-state`,
`set-planning`, `set-priority`, `edit-link`, `add-tag`, `remove-tag`,
`rename-tag` (update), `archive`, `delete` (delete). Read tools (from the routes
table): `get-headline` (`materialize`), `list-headlines` (`headlines`),
`links`, `tags`, `keywords`, `properties`. `delete` and `archive` carry the
existing destructive-op walls (`Commands.hs:144`); MCP surfaces them as tools an
agent may be configured to gate.

Arg schema, per verb, reads off the `Args` record it consumes — e.g. `set-title`
→ `{id, title}` (`agTitle`), `set-state` → `{id, keyword}` (`agKeyword`),
`add-tag`/`remove-tag` → `{id, tag}` (`agTag`), `capture` → the widened cargo
(`agTitle`/`agBody`/`agTags`/`agPlanning`/`agProps`, `Commands.hs:63-70`).

## The one real risk — two writers, two views

A running `serve`/`desktop` daemon **and** an offline `glance mcp` process each
hold their own store view. Files stay safe: the drift-lock digest + optimistic
version + temp-then-rename + `EXTERNAL.jsonl` already guard the write door
against concurrent writers (`docs/invariants.md`, one-writer-per-file). But the
offline walker's *view* goes stale under the daemon's edits — it can refuse a
write on drift it did not cause. So the rule the proposal pins: **prefer the
`/mcp` route (one shared store) whenever a daemon runs; reserve offline `glance
mcp` for the no-daemon case.** `glance mcp` may even detect a daemon (the
`/status` endpoint) and proxy to its `/mcp` rather than boot a second store.

## Shippable increments

1. **`Glance.Mcp` core + `POST /mcp` route.** Catalog from `commands` + read
   verbs; dispatch through the live Hub. This is the smallest useful slice — the
   store already exists on the daemon, every write already runs through it. An
   agent points at `http://localhost:PORT/mcp`.
2. **`glance mcp` stdio subcommand.** Boots the headless store offline (backfill
   pattern), bridges stdin/stdout JSON-RPC to the same core. Zero-config for an
   agent that spawns glance.
3. **Daemon-aware `glance mcp`.** When `/status` says a daemon owns the tree,
   proxy to its `/mcp` instead of booting a second store — folds the two-writer
   risk away.

## Risk

- **Public surface**: adds one HTTP route and one subcommand; no change to the
  existing `/command` door or its verbs.
- **Wire/on-disk**: none — MCP writes go through `replaceSpans`, identical bytes
  to an HTTP `POST /command`.
- **Test baseline**: new route + subcommand need their own cases; the `commands`
  table and `Args` stay pinned by their existing tests, and the catalog derives
  from them, so a drift between catalog and verbs is a test, not a review.
