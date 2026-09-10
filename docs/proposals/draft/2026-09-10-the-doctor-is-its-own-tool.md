# Proposal — the doctor is its own tool

**Status:** draft · **Date:** 2026-09-10 · **Origin:** user, after an agent's
first `list-headlines` over `glance mcp` answered five rows wrapped in the
table's chrome and a `doctor` block — *"perhaps better to split doctor and
other methods?"*

## What holds today

- The doctor is ONE scan at startup. `Glance.Web.serve` runs it before the
  routes open and stashes the summary on the hub (`src-web/Glance/Web.hs:74-77`,
  `stashDoctor` `Store.hs:312-319`). Nothing rescans per request, so an
  agent's repeated listings cost no extra walk.
- The summary rides the `/headlines` envelope globally, beside each door's
  own extra (`Routes.hs:338-340`), and the socket's `set-rows` boot frame
  (`Store.hs:255-268`). `docs/commands.md:209-230` documents this as the
  design: the browser's boot log reads it once and `/status` stays
  liveness only.
- `list-headlines` is `mtHeadlines`, which is the `/headlines` handler's
  answer re-wrapped verbatim (`Routes.hs:229`, `Mcp.hs:254-258`, `value`
  `:305-313`). So the agent receives what the table receives: `columns`
  with badge colours as CSS variables, `actions`, `sort`, `views`, and
  `doctor`, before the first row.

The envelope is right for the browser, which boots off one answer. It is
wrong for an agent, which asks many times and wants rows.

## What an agent hits

1. **Health is inseparable from listing.** The only way to ask "is the index
   clean" is to list something. The only way to list is to receive the
   verdict. An agent that wants to check health before a batch of writes
   lists a throwaway query; one that lists in a loop reads the same eight
   counts on every turn.
2. **Chrome in the context window.** A zero-row answer is ~2.7 KB
   (measured in `2026-09-10-the-graph-an-agent-can-walk.md`, point 3); the
   doctor block is ~300 B of that, the rest is `columns`/badges/`actions`
   the agent never renders. `docs/mcp.md` names this pitfall itself: every
   byte a tool returns is spent from the window.
3. **No door for a repair.** When the doctor reports drift, the fix is a
   rescan or a reindex, which has no MCP verb. A `doctor` tool is where a
   `rescan` argument would naturally sit; inside a list answer there is
   nowhere to put it.

## Proposed change

Two edits to `readTools` (`Mcp.hs:245-258`), both thin, both over handlers
that exist.

### 1. A `doctor` read tool

```
doctor {}  →  { "clean": false, "warnings": [...],
                "parseFailures": 6, ..., "recordless": 29 }
```

The answer is `doctorJSON` of the hub's cached summary, verbatim — the same
object `docs/commands.md` already specifies, so the CLI, the boot log and
the tool keep deriving from one `Doctor`. `McpTools` grows one field,
`mtDoctor :: IO Response`, wired to a new `GET /doctor` route that reads
`hubDoctor` (`Store.hs:278`). The description: *"The index's health as
measured at startup: the clean flag, one sentence per finding, and the
counts."*

Optional, second step: `doctor {rescan: true}` reruns `diagnose` and
re-stashes. That is a write in cost (a corpus walk) and should be a
`writeTool` verb, so it lands only if the command engine gains a `rescan`
command; the read tool does not wait for it.

### 2. `list-headlines` answers rows

`list-headlines` stops re-wrapping the browser envelope and returns

```
{ "total": N, "rows": [ { "id", "title", "state", "priority",
                          "scheduled", "deadline", "tags" } ] }
```

`doctor`, `columns`, `actions`, `sort` and `views` drop off. `total` is the
uncapped count so `limit` is honest. This is the `shape: "rows"` answer the
graph proposal asks for as an opt-in; here it is the default, because no
MCP caller renders a table. `mtHeadlines` keeps calling `headlines` and the
tool projects the envelope down, or `headlines` grows a `shape` query
parameter the browser never sends. Either keeps one core.

If a caller wants the verdict inline, a single `"clean": Bool` beside
`total` is the whole compromise. The eight counts and the sentences belong
to `doctor`.

## Not proposed

- Changing the `/headlines` envelope the browser reads, or the `set-rows`
  frame. The boot log's `doctor:` lines stay as `docs/commands.md`
  describes.
- A per-request rescan. The startup scan is the design (`Web.hs:74`); a
  tool that walked the corpus on every call would turn a cached verdict
  into a cost.
- `/status` growing findings. `commands.md:226` keeps it to liveness and
  readiness, and the split here honours that: health has its own door.

## Cost

- `doctor`: one `Tool` entry, one `McpTools` field, one route reading a
  `TVar`. The suite's read-tool list grows by one name.
- `list-headlines`: one projection from the envelope to `{total, rows}`,
  or one query flag on `headlines`. The `total` needs the uncapped row
  count, which `listRequest`'s cap currently discards (`Routes.hs:232`).
- Test: the MCP suite already checks the catalog against `commandNames`
  for writes; add the same pin for `readTools` and an answer-shape check
  for both tools.

## Evidence

- `src-web/Glance/Web/Mcp.hs:44-48,245-258,305-313`
- `src-web/Glance/Web/Routes.hs:229-232,303,338-340`
- `src-web/Glance/Web/Store.hs:238-268,278,312-319`
- `src-web/Glance/Web.hs:74-77`, `src/Data/Org/Doctor.hs:74-110`
- `docs/commands.md:209-230`, `docs/mcp.md` (pitfalls: tool answers spend
  the window)
- Live call 2026-09-10 against `~/sync/views`: `list-headlines {limit: 5}`
  answered `doctor: {clean: false, parseFailures: 6, drift: 34,
  unindexed: 5, recordless: 29}` plus the table chrome around five rows.
