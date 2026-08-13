# Proposal — a stop's KIND and its org NAME are different things

**Status:** proposed · **Date:** 2026-08-14 · **Origin:** `/domain-modeling`
over `AGENTS.hs`'s glossary, which found the model already states the rule this
code breaks.

## The bug, exactly

`#+begin_list` in a subtree makes `S-RET` insert a list item INSIDE the block,
which corrupts it. The mechanism is three lines of `Scan.elm`:

```elm
-- 802: the scanner MINTS a structural list, as a string
opened = out ++ [ Stop i run.to Composite (Just "list") Nothing ]

-- 847: a BLOCK takes the author's own `#+begin_NAME'
, name = b.name

-- 1163: the item-marker rule decides on that string
if top.name == Just "list" && r.grain == Leaf then
```

One field, `Stop.name` (`Scan.elm:714`), carries two vocabularies:

- a **closed** set the scanner mints — `"list"` and its siblings;
- an **open** set the author writes — whatever follows `#+begin_`.

They share a namespace, so an author can spell a structural kind by accident.
`#+begin_list` is the case that exists in the wild; `#+begin_item` and
`#+begin_table` are the same shape.

## The model already says this

`AGENTS.hs` states the rule as one of its three tiers:

> a closed set is a sum with total functions over it, so a new constructor
> breaks every policy

`RegionKind` obeys it — `Plain | Item | Table | Block | Drawer`, total functions
`greater`, `closes`, `markerFor` over it. `Stop.name` does not, and it is the
same domain. The model and the code disagree; the model is right.

## Proposed change

Split the field, so the closed set is a type and the open set stays a string:

```elm
type alias Stop =
    { from : Int, to : Int, grain : Grain
    , kind : StopKind          -- closed; the scanner mints it
    , orgName : Maybe String   -- open; the author writes it
    , up : Maybe Int
    }

type StopKind = ListStop | TableStop | BlockStop | ParaStop | ItemStop
```

`Scan.elm:1163` then reads `top.kind == ListStop`, which an author cannot spell.

## Blast radius, measured

`Stop.name` does not stay inside the scanner — it crosses the port:

| site | what it does |
|---|---|
| `Scan.elm:714` | the field |
| `Scan.elm:802` | mints `Just "list"` (structural) |
| `Scan.elm:847` | `name = b.name` (author's) |
| `Scan.elm:1095`, `1163` | read it |
| `Doc.elm:233` | the echo phrase, `grain-finer (list 1/3)` |
| `Doc.elm:271` | `case up.name of` |
| `Doc.elm:553` | serialized as `"name"` in the port payload |
| `Doc.elm:749` | becomes a CSS class, `"comp d-" ++ name` |

What is NOT affected, and this is what makes the change safe:

- **no glue reads `.name`** off the port payload;
- **`d-list`, `d-table` and friends have no CSS rule** — only `.d-para`,
  `.d-comp`, `.d-draft`, `.d-item`, `.d-head`, `.d-child` are styled;
- **no test asserts `d-list`** — the browser cases read `d-comp` and `d-item`.

So the class emitted from an author's `#+begin_NAME` is dead output today. It is
also unbounded author text becoming a class name, which is worth stopping on its
own: emit the class from `kind` and leave `orgName` off the DOM.

## What it costs, and what it buys

Two Elm modules, the port payload's shape, `ScanTest.elm`, and a rebuilt
`assets/elm.js` (a committed build input, so `make elm` owes its step). The
model gains a `StopKind` beside `RegionKind`, and `AGENTS.hs` gains the check
that the two agree.

Buys: `#+begin_list` stops corrupting a block, `#+begin_item` and
`#+begin_table` stop being latent versions of the same bug, and an author's text
stops reaching the DOM as a class.

## Risk

The port payload changes shape, and `assets/elm.js` is byte-reproducible and
committed — a source change that skips `make elm` ships a stale program. The
regression case is a fixture with `#+begin_list`, which no fixture has today.

## Alternative considered

Keep one field and make the scanner's minted names unspellable — `"*list*"`,
say. Rejected: it hides a type error behind a naming convention, and the next
reader who adds a structural kind has to know the convention exists. The
namespace is the problem; renaming inside it is not a fix.
