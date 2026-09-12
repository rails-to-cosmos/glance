# Proposal — an MCP read tool is a table row rendered onto its route

**Status:** proposed · **Date:** 2026-09-12 · **Origin:** `/generalizer`, the
variant-cost sweep over the MCP door after `neighbors` landed.

*Line numbers are read against the working tree of 2026-09-12, which carries the
argument-rendering fix described under **What landed first** below.*

## Pattern

The write half of the catalog is already one row per verb: `writeTool`
(`Mcp.hs:290-292`) builds the `Tool` and `asCommand` (`Mcp.hs:295-300`) turns
whatever arguments arrived into a `/command` body, untouched. A thirteenth verb
is one line in `writeTools`, and `mcpWriteToolNames` (`Mcp.hs:196-197`) hands
the suite the roster, so `TestServe.hs:9051-9052` pins it with no hand-typed
list.

The read half pays four sites per tool:

1. **An `McpTools` positional field.** `Mcp.hs:50-57`. Three of the five are
   reads, and after the rendering fix their arguments are all one type:
   `mtHeadlines :: Arg -> Arg -> Arg -> IO Response` (`:53`) is query, cap and
   `edges` as three same-typed neighbours, and
   `mtNeighbors :: Text -> Arg -> Arg -> Arg -> IO Response` (`:54`) is depth,
   cap and kind. **Any transposition in either typechecks** — `limit` read as
   the query, `kind` read as the cap. `LayerWrite`'s docstring
   (`Routes.hs:1011`) names this exact hazard for four fields; these are three
   apiece with no names at all.
2. **A `Tool` entry that reads its own arguments, in that order.** `readTools`
   (`Mcp.hs:250-287`): each handler spells `argBytes "…" args` per argument and
   threads them applicatively into the field's parameter order.
3. **A `Routes.hs` `*Request` synthesizer plus an `mcpToolsFor` lambda.**
   `mcpToolsFor` `:230-239`, `headlineRequest` `:241-244`, `listRequest`
   `:246-253`, `neighborRequest` `:254-260`. Three functions whose whole body is
   `param` over a fixed key list, plus one fixed pair (`shape=rows`) and one
   argument that does not ride the query string at all (`get-headline`'s `id`,
   handed to `materialize` directly at `:233`).
4. **A hand-typed name in the `tools/list` test.** `TestServe.hs:9062-9066` —
   `sort (commandNames <> ["get-headline", "list-headlines", "doctor",
   "neighbors"])`, the one place in the MCP suite that re-spells a roster the
   code owns.

The cost is about to be paid four more times:
`docs/proposals/proposed/2026-09-08-the-commands-are-mcp-tools.md:59-62` names
`links`, `tags`, `keywords` and `properties` as read tools. All four are routes
today (`Routes.hs:163-167`); as tools they are four sites apiece — sixteen edits
for four table rows.

## What landed first

The reading half of this was a live divergence and has been closed, in the tree,
on 2026-09-12. `argInt` read `Just (Number n) -> Just (round n)` and everything
else as `Nothing`, so `{"limit": "5"}` silently meant *no cap* and
`{"limit": 2.7}` meant 3, where `?limit=2.7` and `?limit=abc` are refused by
`wholeNumber` (`Routes.hs:504-510`). The fix is the renderer this proposal
assumed: `argBytes` (`Mcp.hs:346-354`) turns a scalar into the query bytes,
`spelled` (`:358-361`) keeps a number's own decimal spelling, `reading`
(`:364-365`) answers the 400, and `param` (`Routes.hs:262-265`) puts it on the
request. **The tool renders and the route reads.**

What that fix does NOT do is remove the four sites. It made site 1's twins
wider — three `Arg`s in a row where there used to be a `Maybe Text` beside a
`Maybe Int` beside a `Bool`. This proposal is the structural half.

## Files

`src-web/Glance/Web/Mcp.hs:47-57`, `:181-193`, `:250-287`, `:290-300`,
`:335-365`; `src-web/Glance/Web/Routes.hs:148-177`, `:228-263`;
`test/TestServe.hs:9062-9066`.

## Proposed change

**A read tool carries the address of its route; the door dispatches.**

```haskell
-- | A read tool IS its route: the path, the tool arguments that ride the query
-- string and the key each rides under, and the parameters the tool fixes.  ONE
-- DOOR, structurally: there is no handler for a read tool to reach past it.
data ReadRoute = ReadRoute
  { rrPath  :: ![Text]                      -- ^ e.g. @["headlines"]@
  , rrArgs  :: ![(Text, BS.ByteString)]     -- ^ tool argument name, query key
  , rrFixed :: ![(BS.ByteString, BS.ByteString)]  -- ^ e.g. @[("shape", "rows")]@
  }

data Tool = Tool
  { toolName   :: !Text
  , toolDesc   :: !Text
  , toolSchema :: !Value
  , toolRun    :: !(McpTools -> Value -> IO (Status, Value))
  }
```

with `readTool` beside `writeTool`, so a read is as short as a write:

```haskell
-- | A read tool: its arguments become the query string of ROUTE, run and wrapped back.
readTool :: Text -> Text -> [(Text, Value)] -> [Text] -> ReadRoute -> Tool
readTool name desc props required rr = Tool name desc (schema props required) run
  where
    run tools args = case traverse (\(k, key) -> fmap ((,) key) (argBytes k args)) (rrArgs rr) of
      Left why -> refused why
      Right vs -> value =<< mtRoute tools (rrPath rr) (defaultRequest
                    { queryString = [ (k, Just b) | (k, Just b) <- vs ]
                                 <> [ (k, Just v) | (k, v) <- rrFixed rr ] })
```

The four reads become rows:

```haskell
readTools :: [Tool]
readTools =
  [ readTool "get-headline" getHeadlineDesc
      [("id", str "the row id, e.g. FILE.org#3"), ("edges", edgesProp)] ["id"]
      (ReadRoute ["headline"] [("id", "id"), ("edges", "edges")] [])
  , readTool "list-headlines" listHeadlinesDesc […] []
      (ReadRoute ["headlines"] [("query", "q"), ("limit", "limit"), ("edges", "edges")]
                 [("shape", "rows")])
  , readTool "neighbors" neighborsDesc […] ["id"]
      (ReadRoute ["neighbors"]
                 [("id", "id"), ("depth", "depth"), ("limit", "limit"), ("kind", "kind")] [])
  , readTool "doctor" doctorDesc [] [] (ReadRoute ["doctor"] [] [])
  ]
```

Every argument is now named at the one place it is named — beside the schema
property it belongs to — so a transposition is a mis-spelled key rather than a
silent swap. `get-headline`'s `id` joins the query string, where
`materialize` reads it already (`queryId`, `Routes.hs:155`).

**`McpTools` collapses to two fields,** and the six positional `Arg`s go with
them:

```haskell
data McpTools = McpTools
  { mtWrite :: BL.ByteString -> IO Response       -- ^ a @\/command@ body.
  , mtRoute :: [Text] -> Request -> IO Response   -- ^ a synthesized GET, dispatched.
  }
```

`mcpToolsFor` (`Routes.hs:230`) wires `mtRoute` to the route table rather than
to four handlers. **That is the move that makes the invariant structural**: an
MCP read cannot reach a handler except through the path the browser uses, so
*"an MCP tool meets the query string's own walls"* (`docs/invariants.md`) stops
being a convention held by three synthesizers.

**The one real cost**: `named` (`Routes.hs:151-176`) is a let-bound list inside
`application`, closed over `request`. `mtRoute` needs it lifted to
`routeTable :: ServeOptions -> Hub -> Request -> [Route]`, with `application`
folding the same value. No behaviour change; `bootstrapWanted` already takes
that shape.

**The test derives its roster** the way the write half does:

```haskell
mcpReadToolNames :: [Text]
mcpReadToolNames = map toolName readTools
```

so `TestServe.hs:9065` becomes `sort (commandNames <> mcpReadToolNames)` and the
case asserts shape — every tool declares an object schema — rather than
membership.

## LOC estimate

Added ~45 (`ReadRoute`, `readTool`, the route-table lift, `mcpReadToolNames`).
Removed now ~60 (four handler lambdas at `Mcp.hs:250-287`, three
`*Request` synthesizers and `param` at `Routes.hs:241-265`, three `McpTools`
fields and four `mcpToolsFor` lambdas, the hand-typed name list). **Per future
read tool: ~26 lines over three files today, ~6 lines in one table after** — and
the test roster, the argument names and the route address each land exactly
once. The four tools `2026-09-08-the-commands-are-mcp-tools.md:59-62` still owes
are the immediate payer: four rows against sixteen edits.

## Risk

- **API**: `McpTools (..)` and `Arg` are exported (`Mcp.hs:7-8`); `McpTools` is
  built only by `mcpToolsFor` and consumed only by `mcpRoute`/`runMcpStdio`.
  `Arg` loses its only reason to be exported.
- **Wire**: one behaviour change, and it is a narrowing —
  `get-headline`'s `id` starts riding the query string, so an id that is not
  UTF-8-decodable meets `queryWord`'s refusal (`Routes.hs:488-492`) rather than
  `fromMaybe ""`. Everything else answers byte for byte: the same paths, the
  same `shape=rows`, the same refusals `argBytes` and `wholeNumber` already
  give.
- **On-disk**: none. No write road is touched; `writeTool`/`asCommand` stay.
- **Test baselines**: the MCP group (`TestServe.hs:9048`+) asserts answers, so
  it stands — including the argument cases landed 2026-09-12 (*"a cap given as a
  string pages like ?limit=5"*, *"neighbors' numbers are the query string's,
  rounded by nobody"*), which become the proof the dispatch is faithful. The
  browser MCP explorer case counts the catalog; the count is unchanged.

## Existing precedent

- `writeTool` / `asCommand` (`Mcp.hs:290-300`): the write half already passes
  its arguments through the command door untouched, and `mcpWriteToolNames`
  (`:196`) is the roster the suite reads instead of retyping. `readTool` is that
  function's twin.
- `argBytes` / `param` (landed 2026-09-12): the rendering half of the same
  idea — the tool renders, the route reads.
- `linkPlaceOf` over `[minBound .. maxBound]` (`Query.hs:1770-1782`): a closed
  word whose wall, refusal sentence and roads read one vocabulary.
- `docs/invariants.md`, *"an MCP tool meets the query string's own walls"* and
  *"a fact several readers agree on is spelled in ONE list, indexed by key."*
