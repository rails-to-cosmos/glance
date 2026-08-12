# Proposal — one address, a declared kind, an index, and the shape

**Status:** proposed — five decisions taken 2026-08-12; nothing built ·
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
hand with `@` / `C-u @` in the material buffer.

glance's `refTargetOf` strips the prefix and keeps **everything after it**:

```haskell
refTargetOf "org-glance-material:contact-25053-3?kind=author"
  == Just "contact-25053-3?kind=author"     -- resolves to nothing, ever
```

That is why `org-glance-material:` dangles at **34.5%** where `visit:` dangles
at 11.6%. Every typed reference the peer writes is invisible to `ref:`, no test
covers `?kind=`, and the blast radius grows with every `C-u @` pressed.

And `Data.Org.Index` reads 4 of the 16 keys org-glance writes per record.
`relations` — typed, directed, deduplicated — sits in the same already-parsed
JSON object and is dropped on the floor.

**So decision 2 is not speculative.** `RefKind` is what stops glance losing
edges the peer is already writing.

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

### The migration, and why it is separate

Rewriting ~4435 links in the user's real tree is a change to their files, so it
is its own staged step with its own review: a `glance migrate-links --dry-run`
printing every rewrite, then the same command writing through the ordinary
`Data.Org.Edit` splice — optimistic lock, temp-plus-rename, every other byte
identical. It is reversible by `git` in a tree under version control and by the
trash in one that is not.

**It is not required for anything else here to land.**

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
| 6 | **`GET /graph`, scoped by `?q=`**                                                               | M2, and it is what 4 was for                                         |
| — | **`migrate-links`, dry-run first**                                                              | optional forever; rewrites the user's files                          |

Stages 1 and 2 are a day and pay immediately. Stage 6 is the milestone.

## What this does not buy

- **A verb.** Nothing here lets a reader *make* a link, and **0 of 3524** edges
  in the corpus could have been made from glance. That is a separate proposal
  (`@` over the document pane, a `/refer` picker, the link composed
  server-side) and it is the only item on any list that can move 0.34 edges per
  row. Everything in this document makes existing edges better understood.
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
