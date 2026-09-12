# Proposal — the commands are already MCP tools

**Status:** partial — the three shippable increments landed (stages 1 and 2 in
the 2026-09-08 stack, stage 3 the day after; `docs/tasks.org`); the section *the
tool catalog, derived not hand-written* did not · **Date:** 2026-09-08 ·
**Origin:** user — *"especially for agents. So perhaps it would be good to have
'glance mcp' cli and api route, what do you think?"*

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


## 2026-09-12 update — what landed, and the one step left

**Landed.** All three increments under *Shippable increments*:
`Glance.Web.Mcp` and `POST /mcp` (`Routes.hs:171-172`), `glance mcp` over stdio
(`app/Main.hs:100`, `runMcpStdio`), and the daemon-aware proxy (`mcpDaemonAt`,
`/status` carrying the served `dir` at `Routes.hs:226`). The catalog grew four
read tools rather than the six this file named, and the graph tools it did not
foresee — see `docs/proposals/done/2026-09-10-the-graph-an-agent-can-walk.md`.

**Not landed: the catalog is still hand-written.** This file's section at
`:50-68` says *"a new verb in `commands` is a new MCP tool with no second
edit."* It is a second edit today, and a third:

- `writeTools` (`Mcp.hs:199-247`) hand-spells every verb's description, every
  argument's JSON-Schema type and every `required` list. Twelve rows, ~48 lines.
- `Commands.commands` (`Commands.hs:149`) holds the same twelve verbs with
  `csArgs` — the request-shape guard — and no description, no property types.
- `AGENTS.hs:3388-3407` holds them a THIRD time as `cmds`, whose `cArgs`
  (`AGENTS.hs:3377`) carries each argument's name and arity (`Req`/`Nul`/`Opt`).

`mcpWriteToolNames` (`Mcp.hs:196`) pins the NAMES against `commandNames`
(`TestServe.hs:9051-9052`), and `TestSpec.hs:976` pins `cWire` against them too,
so no verb can be missing from any of the three. **The ARGUMENTS are pinned
across two of the three and not the third.** `TestSpec.hs:1016-1029` drives
`cArgs` against the live `/command` door — one null per field, asserting a `Nul`
field clears and a `Req` field is refused — so the model and the engine agree.
Nothing compares either to `writeTools`' schema. A verb that gains an argument
gets it in `Commands.hs` and `AGENTS.hs`, and an agent reading `tools/list`
never learns the argument exists.

### The remaining step

`CommandSpec` (`Commands.hs:124-129`) carries what the catalog needs:

```haskell
-- | ONE ARGUMENT: the wire name, what it takes, whether it is owed, and the
-- sentence the catalog shows.  The request-shape guard reads it, and so does
-- the MCP schema, so a verb cannot document one and take another.
data CommandArg = CommandArg
  { caName :: !Text
  , caType :: !ArgType          -- ^ Str | StrOrNull | Arr Str | Pairs | Span
  , caNeed :: !Arity            -- ^ Req | Nul | Opt, `AGENTS.cArgs`' own word
  , caHelp :: !Text
  }

data CommandSpec = CommandSpec
  { csDesc  :: !Text            -- ^ the tool description, one sentence.
  , csProps :: ![CommandArg]
  , csArgs  :: [Text] -> Args -> Maybe Text
  , csAsks  :: Asks
  , csKind  :: CommandKind
  }
```

and the catalog folds it:

```haskell
writeTools :: [Tool]
writeTools =
  [ writeTool n (csDesc s) (schemaOf (csProps s)) [ caName a | a <- csProps s, caNeed a == Req ]
  | (n, s) <- commands ]
```

`csArgs` can then be DERIVED from `csProps` for the verbs whose guard is only
arity (nine of twelve); `capture`'s either-or road and `edit-link`'s span stay
hand-written, which is what `csArgs` is for.

`TestSpec` gains the third diff beside its route, command and column ones:

```haskell
testCase "the spec's arguments are the engine's" $
  assertEqual "an argument moved"
    [ (Spec.cWire c, f, a) | c <- Spec.cmds, Spec.Arg f a <- Spec.cArgs c ]
    [ (n, caName p, arityWord (caNeed p)) | (n, s) <- commands, p <- csProps s ]
```

**LOC**: added ~60 (`CommandArg`, `ArgType`, `schemaOf`, twelve `csDesc`/
`csProps` rows), removed ~55 (`writeTools`' twelve hand-spelled schemas, nine
`csArgs` bodies). **Per future verb: one `commands` row instead of a
`commands` row plus a `writeTools` row plus an `AGENTS.cmds` row.**

**Risk**: `tools/list`'s `inputSchema` is a wire surface an agent's client
caches. Derivation must reproduce today's bytes for the twelve verbs, which the
browser MCP explorer case and `TestServe`'s catalog cases are the check on.
No write path, no org bytes.
