# Proposal — additive filters: `+key:value` widens its own axis

**Status:** proposed · **Date:** 2026-08-20 · **Origin:** user — *"`priority:[#B]`
reads: filter the current set by priority=B. `+priority:[#B]` should read: add
priority=B headlines to the current set, considering other filters."*

## The law in one line

A token opening with `+` joins its key's OWN axis as an alternative: within one
axis the plain tokens AND as today and the `+` tokens OR against that
conjunction; the axes still AND with each other.

```
tag:work priority:[#A] +priority:[#B]
```
serves the `work` rows whose priority is A **or** B — the addition widens the
priority axis and leaves every other filter standing, which is the "considering
other filters" of the ask. Today the same set needs the `|` spelled inside one
token (`priority:A|B`); `+` says it across tokens, which is what an
incremental gesture — a second click on a badge, a chip added to a live query —
can actually append.

## Grammar

- `+` is a prefix in the `-` position: the token's first character, outside
  quotes. `+` and `-` are mutually exclusive on one token (`+-x` reads as
  free text, the resolver's usual fallthrough).
- Every narrowing key takes it: the six column keys, `planned`, `ref`,
  `substring` — and bare free text, whose axis is `substring`'s: `milk +bread`
  serves rows carrying either word.
- Alternatives ride along: `+state:DONE|CANCELLED` joins both.
- Metas ride along: `state:TODO +state:*inactive*`.
- `+sort:…`, `+columns:…`, `+view:…` are refused the way `-sort:…` is —
  order and shape never narrow, so they have nothing to widen. Same 400 shape:
  `an order key cannot be added: '+sort:title'`.

## The axis rule, precisely

Group a query's narrowing tokens by field (the six columns, `planned`, `ref`,
and `substring`-plus-free-text as one axis). Per axis:

- no `+` tokens → the conjunction of the axis's tokens, exactly today's law
  (`state:TODO state:DONE` still serves nothing);
- `+` tokens present → `(conjunction of the plain and negated tokens) OR
  (disjunction of the + tokens)`; an axis holding only `+` tokens is just the
  disjunction, so a lone `+tag:work` is `tag:work`.

Axes AND, unchanged. Negation stays inside the conjunction half:
`-state:TODO +state:DONE` is "not TODO, or DONE" — the widening reading; a
reviewer who wants `+` refused beside a negated same-key token can have that
instead, one guard in the same place.

Worked examples against the pinned corpus:

| query | serves |
| --- | --- |
| `priority:[#A] +priority:[#B]` | priorities A and B |
| `tag:work priority:[#A] +priority:[#B]` | work rows at A or B |
| `tag:work +tag:home` | rows tagged work or home (today: the intersection) |
| `+state:DONE` | ≡ `state:DONE` |
| `planned:*empty* +planned:2026-08` | undated rows plus the August ones |
| `ref:A +ref:B` | rows referring to either |
| `milk +bread` | rows carrying either word |

## Implementation sketch

- `Filter.hs`: `Token` gains `tkAdded :: Bool` beside `tkNegated`
  (`scanQuery`'s `-` branch gets a `+` twin, same first-char rule). `resolve`
  carries it onto `Term`. `matchesFilter` stops folding one flat `all` and
  folds per-axis groups with the rule above — the axis is `fieldOf`'s answer,
  already computed. `Sort.hs`/`Columns.hs` refuse `+` where they refuse `-`.
- The wire changes nothing: `?q=` already carries the string; `X-Glance-*`
  and paging are untouched.
- The renderer (`table-view.js`) reads `+state:B` as free text today — a new
  row in `AGENTS.hs`'s divergence table (`AddKey`, renderer narrower), the
  tripwire's key-dropping probe already covers the shape. Parity vectors gain
  a `+` case in `fixtures/parity/filter-query.json`.
- Tests: TestFilter — the axis table above plus: `+` beside `|`, `+` with an
  empty value (narrows nothing, adds nothing), `+STATE:x` stays free text,
  quoted `"+state:x"` stays free text, `+substring:"-x"`; TestServe — one
  keyed drive with a `+` query over the wire; README's table gains one row
  and `docs/query.md` a section.

## Alternatives considered

- **Spelling `|` across tokens automatically** (same-key plain tokens OR
  instead of AND): silently flips the pinned `tag:work tag:glance`
  intersection law — rejected.
- **A general `or:(…)` group combinator**: strictly more expressive, much
  more grammar (nesting, precedence, quoting), and the incremental-gesture
  case never needs it — deferred, and `+` does not block it later.

Inert until reviewed.
