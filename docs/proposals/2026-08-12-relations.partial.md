# Proposal — one address, a declared kind, an index, and the shape

**Status:** partial — five decisions taken 2026-08-12.  One slice of stage 1
landed 2026-08-15: `refTargetOf` now cuts a protocol target at its first `?`,
so an edge the peer wrote as `…:ID?kind=SLUG` resolves to `ID` rather than to
nothing (`TestFilter.hs`, "a kind suffix rides off a protocol target").  A
title keeps its own `?`.  **Stage 5b's LINK half landed 2026-08-16** — `@` in
the sheet and `GET /refer`, see
[`2026-08-15-a-relation-is-a-link-with-a-kind.partial.md`](2026-08-15-a-relation-is-a-link-with-a-kind.partial.md);
its KIND half waits on stage 1.  The `Ref` TYPE, `refKind` on the wire, the
reverse index, `/graph` and `glance migrate` are all still unbuilt ·
**Date:** 2026-08-12 · **Origin:** the relations census over `~/sync`
(10452 rows, 3524 edges), and the user's framing of the project as *org-mode
under the hood, source of truth, the user operates views: filtering / schema
definition / relations* — where relations is the leg that is not yet a verb.

## The decisions this implements

1. **One protocol.** `glance:` replaces `org-glance-visit:`,
   `org-glance-open:`, `org-glance-material:` and `id:`.
2. **`RefKind` stays.**
3. **In-edges get an index.**
4. **Resolution is id-based only** — the two title shapes go.
5. **Shape queries land in glance** — two-hop, paths, orphans, `GET /graph`.
6. **The kind keeps the peer's `?kind=` spelling** — `glance:ID?kind=SLUG`.
   Only the scheme moves.
7. **`@` in the materialize sheet makes a reference** — a popup completing over
   headlines, then an optional kind completing over the kinds the tree already
   uses. This is the verb the census says is missing.
8. **The link rewrite is one entry in a global `glance migrate --dry-run`**,
   which migrates schemas generally rather than links alone.

## What the corpus says

Measured through `Glance.Query.loadDir` itself, so the census and the product
share one rule. 10452 rows, 6302 files, 14.0 s.

|                                                          |                                           |
|----------------------------------------------------------|-------------------------------------------|
| edges (source → target, both rows)                       | **3524**, or 0.34 per row                 |
| rows touching no edge at all                             | **8648 / 10452 = 82.7%**                  |
| rows pointed at by nothing                               | **9532 / 10452 = 91.2%**                  |
| rows carrying *some* link                                | 4988 = 47.7%                              |
| rows whose links go nowhere a table follows              | 3161 = 30.2% (3480 `https`, 1232 `file:`) |
| rows carrying no `ORG_GLANCE_ID`                         | **4397 = 42.1%** — unaddressable          |
| edges with both ends under `.org-glance/data/`           | **3517 / 3524 = 99.8%**                   |
| edges contributed by the 4388 rows in ordinary org files | **7**                                     |

Protocol mix over the 4476 row-shaped links:

| protocol               | count | share |
|------------------------|-------|-------|
| `org-glance-visit:`    | 3867  | 86.4% |
| `org-glance-open:`     | 568   | 12.7% |
| `org-glance-material:` | 29    | 0.65% |
| leading `*`            | 6     | 0.13% |
| bare `[[Title]]`       | 6     | 0.13% |
| `id:`                  | **0** | 0%    |

Resolution, which settles decision 4 on its own: **3523 of 3524 edges matched
an `ORG_GLANCE_ID`. Exactly one matched a title** — `[[*Known hazards]]`,
pointing inside its own file, in org-glance's own documentation. Of the 6 bare
targets, **0** resolve; of the 6 star targets, 1 does. So the two title shapes
cost 11 false positives to buy 1 true edge.

## The finding that reorders everything

**The peer already shipped typed edges, and glance silently drops them.**

org-glance's relation model landed 2026-07-18. Its canonical edge is
`[[org-glance-material:ID?kind=SLUG][Title]]`, kinds slugged on both encode and
decode, deduplicated by `(target, kind)`, stored in the WAL's `:relations`
field, rendered in both directions, filterable by `:refers-to`, and inserted by
hand with `@` / `C-u @` in the material buffer (the PEER's spelling, in Emacs,
where `C-u` is the universal argument; glance's own is settled elsewhere).

glance's `refTargetOf` kept **everything after the prefix**, so a typed edge
resolved to nothing, ever — which is why `org-glance-material:` dangled at
**34.5%** where `visit:` dangled at 11.6%.

**That half is FIXED** (`66cec36`, 2026-08-15). `refTargetOf` now cuts at the
first `?` on the protocol branch alone — a title's own `?` is text and stays —
so a typed edge resolves today (`src-query/Glance/Query.hs:552-563`, pinned at
`test/TestFilter.hs:88-101`).

What remains is the half this proposal is named for: **the kind is stripped and
then DISCARDED**. `refTargetOf` answers `Maybe Text`, so `depends` exists
nowhere downstream, and `refTargetsOf` dedups on the id alone where the peer
dedups on `(target, kind)` — two typed edges to one row collapse into one. The
kind survives in exactly one place, unparsed: the raw `olTarget`, which `/links`
ships verbatim.

And `Data.Org.Index` reads 4 of the 16 keys org-glance writes per record.
`relations` — typed, directed, deduplicated — sits in the same already-parsed
JSON object and is dropped on the floor.

**So decision 2 is not speculative.** `RefKind` is what stops glance losing
edges the peer is already writing.

### What the store actually holds, counted 2026-08-17

Re-counted against `~/sync` (6,345 `.org` files, 6,098 distinct ids) because
the figures above were computed ad hoc and no census tool is checked in:

| | |
|---|---|
| `?kind=` in the store's org files | **5** occurrences, all `roasted-by`, all one authored edge re-rendered by the overview and agenda writers |
| WAL records carrying `relations` | 61 |
| relation entries in the WAL | 92, of which **5 carry a kind** |
| the whole kind vocabulary | `roasted-by` ×4, `by` ×1 — and `by` is an earlier revision of the same coffee row |

**One authored typed edge in the corpus.** That reorders the design of the kind
stage rather than the model: `kinds` folded off the store answers a one-element
list, so the completion has nothing to complete against and **free text is the
primary path, not the fallback**. Every choice about it — the wall, the slug,
what the echo says — is load-bearing from the first press, where the proposal
had treated it as a convenience ("free text accepted so a new kind costs no
configuration").

## The design

### `glance:` addresses, the kind is declared

The four old protocols were never relation kinds. `visit:`, `open:` and
`material:` are **verbs about how Emacs opens the target**; `id:` is org's
addressing. None says how two entries relate, which is why collapsing them
loses no meaning.

That frees `RefKind` to mean the thing the user's own framing asks for under
*schema definition*: **the kind of relationship, declared by the author**, the
way a tag declares a type.

```org
[[glance:7db7af20-…][the entry]]              a plain mention, kind-less
[[glance:7db7af20-…?kind=depends][the entry]] a typed edge
```

The kind rides the link because that is where the sentence explaining the
reference already lives — `refTargets`' own comment says a reference is nearly
always written in the body, *"where a reader puts the sentence that explains
it."* A drawer property would move the edge away from its explanation.

**`?kind=` is the peer's own spelling and is kept verbatim.** The scheme is the
only thing that moves, which buys three things: the peer's decoder
(`--link-edge`) and its slug rules are untouched, so its half is one handler
plus one string in the encoder; every edge already in the corpus differs from
its new spelling by a prefix alone, so the migration is a pure prefix swap; and
one grammar is spelled once across two programs, where a second spelling would
be a second thing to keep in step. `refTargetOf` cuts the target at the first
`?` — org ids are UUIDs, so no target carries one of its own.

### `@` in the sheet makes a reference — the verb

`@` over the materialize sheet's document pane inserts a reference, mirroring
the peer's own double binding of the same key: `org-glance-overview:relations`
in the overview, which is glance's table today, and `org-glance-material:refer`
in the material buffer, which is this. One key, two surfaces, the same split
the peer already made.

**The popup is the capture form's shape**, which is the closest thing already
built: a field with the tree's vocabulary narrowing under it, at most eight
rows, `C-n`/`C-p` and the arrows walking a highlight, `RET` taking it.

1. **the headline** — narrowed over the store, addressable rows only;
2. **the kind**, optional — narrowed over the kinds the tree already uses,
   free text accepted so a new kind costs no configuration. **`k` while the
   picker is up asks for it**, settled 2026-08-17: a prefixed `@` cannot be
   pressed over a selected region, which is the gesture layer 1 exists for. See
   ["The chord that could not be
   pressed"](2026-08-15-a-relation-is-a-link-with-a-kind.partial.md).

`RET` moves forward and commits at the last field; an empty kind is a plain
mention; `ESC` cancels through `SURFACES` like every other momentary surface.

**One endpoint.** `GET /refer?q=TEXT[&kind=SLUG][&limit=N]` answers
`{rows: [{id, title, insert}], kinds: [{kind, rows}]}`.

- `q` is narrowed by `Glance.Web.Filter`'s own `compile` over `hrSearch` — the
  same grammar the table takes, so there is no second matcher.
- **Addressable rows only.** A row with no `ORG_GLANCE_ID` cannot be linked to,
  so the 42% wall is expressed as a filter rather than as a refusal a reader
  meets after choosing. The row the sheet stands on is dropped too: a row is
  not its own reference.
- `kinds` is the store-wide vocabulary with a row count each, the shape `/tags`
  already answers in, folded off `refKind` once `RefKind` lands. This is why
  the verb is staged after stage 1.
- `insert` is **composed server-side** under the current `kind`, so the page
  still spells no bracket grammar and the peer's slug rule is applied where it
  is already implemented. Changing the kind re-fetches, which the completion is
  doing anyway, so `insert` is never stale.

**Where the link lands — two modes, and both already exist.**

- **A paragraph edit is open:** insert at the caret. The reader is writing the
  sentence that explains the reference, which is where a reference belongs.
- **No edit open:** `+`'s own path — `Scan.joinAt` picks the landing by grain,
  a zero-width draft row is drawn, and the box is seeded with `insert` instead
  of an item lead. The reader types prose around it and `RET` commits.

**So the write path is untouched.** The commit is the sheet's existing
drift-locked `POST /headline {body, properties, planning, digest}` — 409 on
drift, `untrailed`, one owner per byte. A box still holding only its seed
writes nothing, which is `+`'s rule unchanged. **No twelfth command is owed**:
placement is what makes a headless `add-link` hard, and the sheet is the one
surface with a point.

**Refusals.** No addressable match is one `cmd info` line. Over the property
panel or a child headline, `@` refuses and names the pane. Target existence
needs no wall — it was picked out of the store.

**Echo.** `@ → org-glance-material:refer (Wrike MDE Team · author)`, the kind
omitted where there is none.

**Tests.** `/refer` narrows exactly as `/headlines?q=` does over one fixture; a
row with no `ORG_GLANCE_ID` never appears; the sheet's own row never appears;
`insert` **reparses** to the id and description it names, which is `spelling`'s
own reparse-and-compare idiom. In the harness: `@` raises the picker, a pick
seeds the draft, `RET` fires exactly one `POST /headline`, a seed-only box
fires none. In `make interop`: an edge written here decodes through
`org-glance--link-edge` to the `(target . kind)` it names.

### Resolution: id only

```haskell
data Ref = Ref { refTarget :: !Text, refKind :: !(Maybe Text) }

refTargetOf :: Text -> Maybe Ref   -- glance:ID[?kind=SLUG] and the aliases; nothing else
```

`refSpellings` drops to the `ORG_GLANCE_ID` alone. `[[*Title]]` and
`[[Title]]` stop being references. Cost, measured: **1 edge**. Gain: 11 false
positives removed, and a rule a reader can state in one sentence.

`refKind` is `Maybe Text` rather than a closed sum — kinds are the tree's
vocabulary, minted by free input, so no build can enumerate them. The peer's
slug rule (downcase, whitespace runs → `-`, applied on both encode **and**
decode) must be mirrored, or `Author` and `author` become two kinds across the
wire.

### Reading the old spellings, forever

`refPrefixes` becomes: `glance:` canonical, the four old ones **deprecated
aliases that never stop being read**. One list entry each. A corpus written
over years keeps resolving with no migration run at all, which is what makes
the migration optional rather than a flag day.

### `glance migrate` — one command, a registry of schemas

Rewriting ~4435 links in the user's real tree is a change to the source of
truth, so it is its own step with its own review. It is also **not the only
schema this tree will outgrow**, so the link rewrite is one entry in a general
command rather than a command of its own.

```
glance migrate [--only NAME] [--write] <roots>
```

**Dry run is the default.** A bulk rewrite over the source of truth is the one
place in this repo where the safe mode should need no flag; `--write` applies.
The report is `scan`'s shape — per migration, the files it would touch and the
sites inside them, then a total.

**A migration is a registry row**, the idiom `commands`, `configSettings` and
`savedViews` already use — one list, many readers, so a new migration is one
entry rather than an edit in four places:

| field | |
|---|---|
| name | what `--only` takes and what the report prints |
| what it looks for | a predicate over a parsed document |
| the edits | a span edit set per file |
| what it says | one line per site, for the dry run |

Every migration writes through `Data.Org.Edit` — optimistic lock,
temp-plus-rename, every other byte identical — so a migration inherits the
surgical property rather than restating it, and no migration may rewrite a line
it was not handed. One file is one atomic write; there is no cross-file
transaction, exactly as `/command` has none.

**Two rules every entry owes**, and the suite quantifies over the registry the
way `TestConfig` does over `configSettings`:

- **idempotence** — running twice changes nothing the second time, so an
  interrupted run is resumed by running again;
- **a fixture** — a document before and after, plus the dry-run line it prints.

**The first entries:**

| name | rewrites |
|---|---|
| `links` | the four old protocols to `glance:`, `?kind=` preserved — a pure prefix swap |

**Candidates the corpus already suggests**, each its own decision and none of
them owed by this proposal: the four superseded `ORG_GLANCE_ID` generations the
corpus carries, `#+SEQ_TODO:`/`#+TYP_TODO:` to `#+TODO:` (the parser already
folds them and re-renders the modern spelling), and the 11 links whose target
is the literal string `nil`.

It is reversible by `git` in a tree under version control, and **it is not
required for anything else here to land** — the aliases make the link
migration optional forever.

### The peer's half

org-glance owns four `org-glance-link:*` handlers and writes
`org-glance-material:` as its canonical edge. Because `?kind=` is kept, it owes
only a `glance:` handler and one string in `--edge->link-path`. Its decoder,
its slug rules and its `(TARGET . KIND)` model are untouched — the scheme is
the whole of the change.

`make interop` gains the case that pins it: an edge written by one side decodes
to the same `(target, kind)` on the other.

### The in-edge index

`ref:` is already 1.42 ms and the index makes it 2.39 µs — a 595× that no
reader perceives, at 40 ms of build and 2.3 MB. **Speed is the wrong
justification.** Two real ones:

- **decision 5 needs it.** A graph endpoint folds both directions by
  definition; building reverse adjacency per request over 10452 rows is the
  thing an index exists to avoid.
- **in-degree becomes showable.** `hrLinked` is a Bool today. A count makes
  "3 things point here" visible without a query — and per the census it would
  print a number on 8.8% of rows and nothing on the rest, which is honest
  rather than disappointing.

One fold at load into `Map Text [RowId]`, maintained per file on the watch step
exactly as the store already is.

### The shape, scoped

99.8% of edges have both ends under `.org-glance/data/`, and 91.2% of rows have
in-degree zero. **A whole-store graph would render 9532 isolated dots.** So
`GET /graph` takes the same `?q=` the table takes and answers over the matched
subgraph plus its one-hop neighbourhood, defaulting to the blob store. Orphans
are a *query* (`-linked:*empty*`-shaped), never a canvas full of dust.

Two-hop and path queries extend the filter grammar rather than the endpoint:
`ref:ID` is one hop today, and `ref:ID/2` or `path:A..B` reads off the same
index.

## Staging

Each stage is independently shippable and independently revertible.

| # | stage                                                                                           | why here                                                             |
|---|-------------------------------------------------------------------------------------------------|----------------------------------------------------------------------|
| 1 | **Strip the kind in `refTargetOf`; `Ref` type; `RefKind` on the record and sparse on the wire** | stops losing the peer's typed edges today; one bug fix and one field |
| 2 | **Id-only resolution**                                                                          | a deletion; costs 1 edge, removes 11 false positives                 |
| 3 | **`glance:` canonical, four aliases read forever**                                              | no corpus change, no peer change yet                                 |
| 4 | **Reverse index + in-degree on the wire**                                                       | prerequisite for 6                                                   |
| 5 | **The peer writes `glance:`; interop case pins the round trip**                                 | two-repo, gated on 3                                                 |
| 5b| **`@` in the sheet, `GET /refer`** — the verb.  THE LINK HALF IS DONE (2026-08-16); the kind half is not | gated on 1 for the kind vocabulary; the only item that moves 0.34 edges per row |
| 6 | **`GET /graph`, scoped by `?q=`**                                                               | M2, and it is what 4 was for                                         |
| — | **`glance migrate`, dry run by default**                                                        | optional forever; rewrites the source of truth                       |

Stages 1 and 2 are a day and pay immediately. Stage 5b is the one a reader
notices. Stage 6 is the milestone.

## What this does not buy

- **Direction on the wire.** `ref:` stays incoming, the link popup outgoing.
  The peer draws `> kind` / `< kind` for a mutual pair; glance will not.
- **Deleting an edge.** Still "edit the text around it."
- **An address for 42% of rows.** 4397 rows carry no `ORG_GLANCE_ID`, so
  nothing can link to them and no graph can draw them.
- **Any change to the 30.2% of rows** whose links are web bookmarks and file
  attachments. They stay underlined and unfollowable by the table.

## Open decisions

1. **`id:` recognition.** Zero occurrences in the corpus, so dropping it is
   free today — and it is *org's own* protocol, so a future `org-id` link would
   stop resolving. Recommendation: keep reading it as a fifth alias. It costs
   one list entry.
2. **Whether the migration runs at all.** The aliases make it optional. The
   argument for running it is one spelling in the tree; against, it rewrites
   4435 links in files that are the source of truth.
