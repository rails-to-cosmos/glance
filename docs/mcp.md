# MCP

A primer on the Model Context Protocol, tied to this repo's server in
`src-web/Glance/Web/Mcp.hs`. The `glance mcp` command is documented in
[commands.md](commands.md); this file is the protocol itself.

## What it is

An open standard (Anthropic, late 2024) for plugging tools and data into LLM
clients. A server exposes capabilities once and any client speaks the same
wire protocol: Claude Code, Claude Desktop, Cursor, Zed, or a custom agent.

Two roles:

- **Client** lives inside the LLM host. It discovers what a server offers and
  forwards the model's calls.
- **Server** is your process. It wraps a thing (a database, a filesystem, an
  API, glance's org store) as protocol primitives.

## Wire format

JSON-RPC 2.0. Every message is
`{"jsonrpc":"2.0","id":N,"method":"...","params":{...}}` and a response carries
the same `id`. A notification has no `id` and gets no reply. Glance handles
this in `rpcResult`, `rpcError`, and the empty 202 for notifications
(`Mcp.hs`, around `mcpHandle`).

Lifecycle:

1. `initialize`: the client sends its protocol version and capabilities; the
   server echoes the version it supports plus its own capabilities
   (`initialized` in `Mcp.hs`).
2. `notifications/initialized`: the client says it is ready.
3. Normal traffic.

## The three primitives

| Primitive     | Driven by       | Methods                            | Meaning                                                                                                                                                                          |
|---------------|-----------------|------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **Tools**     | the model       | `tools/list`, `tools/call`         | functions the model may invoke. Each has a name, a description, and a JSON Schema `inputSchema`. The result is a `content` array (text, image, resource) plus an `isError` flag. |
| **Resources** | the app or user | `resources/list`, `resources/read` | read-only data addressed by URI (`file:///…`, `glance://…`). Context, not actions.                                                                                               |
| **Prompts**   | the user        | `prompts/list`, `prompts/get`      | reusable templated messages; clients surface them as slash commands.                                                                                                             |

Glance implements tools only. Most servers do.

The description text matters more than the schema: the model picks a tool by
its description, so a vague one gets misused.

## Transports

- **stdio**: the client spawns the server as a child process and exchanges one
  JSON message per line over stdin and stdout. Simplest. Logging must go to
  stderr; one stray print to stdout corrupts the stream. Glance: `runMcpStdio`,
  behind `glance mcp`.
- **Streamable HTTP**: a single `POST /mcp` endpoint. The response is plain
  JSON or an SSE stream for long operations, and the server may also accept
  `GET` for server-initiated push. Glance: `mcpRoute`, the "single-JSON
  subset", with no SSE and no push. The spec allows that.
- The older **HTTP+SSE** transport with two endpoints was deprecated in 2025.

## Design points in this repo

The module header says it: ONE CORE, injected transports. `mcpHandle` maps one
message to one response; `mcpRoute` (HTTP) and `runMcpStdio` (stdio) both call
it. Tools reuse the `/command` and `/headline` handlers through the `McpTools`
record, so there is no second write path. The suite checks that the write
tools equal `commandNames`, so the catalog cannot drift from the engine. The
general lesson: keep the MCP layer a thin adapter that owns no logic.

`glance mcp` is daemon-aware: when a ready daemon already owns the port, the
stdio process proxies to its `POST /mcp` (`mcpDaemonAt`, `runMcpStdioWith`)
instead of opening the tree a second time.

## Client side (Claude Code)

```
claude mcp add glance -- glance mcp --dir DIR          # stdio
claude mcp add --transport http glance http://localhost:PORT/mcp
claude mcp list
```

Config lands in `~/.claude.json` (user scope) or `.mcp.json` (project scope,
committable). Tools show up as `mcp__<server>__<tool>` and prompt for
permission per tool unless allowlisted.

## Pitfalls

- stdout pollution on the stdio transport.
- A huge `tools/list`: every description is spent from the context window on
  every turn. Keep the catalog small and the descriptions tight.
- Returning a tool failure as a JSON-RPC error instead of a result with
  `isError: true`: the model never sees the message and cannot recover.
  Protocol errors (a malformed params shape) are RPC errors; tool failures are
  results.
- Auth on the HTTP transport: the spec says OAuth 2.1, most local servers skip
  it. Do not bind to `0.0.0.0` without it.
- Stateful sessions over HTTP need the `Mcp-Session-Id` header; glance's
  stateless subset avoids that.

## Further reading

- Spec: <https://modelcontextprotocol.io/specification>
- SDKs: `@modelcontextprotocol/sdk` (TypeScript), `mcp` (Python). Haskell has no
  official one, hence the hand-rolled module here.
