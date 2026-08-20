# Proposal — additive filters: `+key:value` widens its own axis

**Status:** done — DELIVERED 2026-08-20 · **Date:** 2026-08-20 · **Origin:**
user — *"`priority:[#B]` reads: filter the current set by priority=B.
`+priority:[#B]` should read: add priority=B headlines to the current set,
considering other filters."*

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
  quotes. The sign is that character alone, so a second one is body text
  (`+-x` adds the free-text needle `-x`).
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

Axes AND, unchanged. Grouping is by KEY, never by adjacency, so token order
carries nothing: `priority:[#A] tag:book +priority:[#B]` reads
`(priority:[#A] OR priority:[#B]) AND tag:book` however the three are
interleaved. Negation stays inside the conjunction half:
`-state:TODO +state:DONE` is "not TODO, or DONE" — the widening reading; a
reviewer who wants `+` refused beside a negated same-key token can have that
instead, one guard in the same place.

Worked examples against the pinned corpus:

| query                                   | serves                                             |
|-----------------------------------------|----------------------------------------------------|
| `priority:[#A] +priority:[#B]`          | priorities A and B                                 |
| `tag:work priority:[#A] +priority:[#B]` | work rows at A or B                                |
| `priority:[#A] tag:book +priority:[#B]` | book rows at A or B — order never matters          |
| `tag:work +tag:home`                    | rows tagged work or home (today: the intersection) |
| `+state:DONE`                           | ≡ `state:DONE`                                     |
| `planned:*empty* +planned:2026-08`      | undated rows plus the August ones                  |
| `ref:A +ref:B`                          | rows referring to either                           |
| `milk +bread`                           | rows carrying either word                          |

## Formal semantics

The narrowing language with `+`, precisely. Shaping tokens (`sort:`,
`columns:`, `view:`) order and shape the answer, never narrow, and refuse
both signs — they are outside this section.

### Grammar

```
query   ::= token (WS token)*
token   ::= sign? key ":" value        -- a narrowing token
          | sign? word                 -- free text
sign    ::= "+" | "-"                  -- at most one, the token's first char
value   ::= alt ("|" alt)*             -- alternatives, today's own
```

The resolver's fallthrough stands: anything failing the shape — an
upper-case key, a quoted `"+state:x"` — reads as free text, the sign
surviving where the key does not. Signs never nest and never quote: a second
sign is body text, so `+-x` adds the needle `-x`.

### Denotation

Fix a row set `R`. Every narrowing token `t` has today's atomic predicate
`⟦t⟧ : R → Bool` ([query.md](../../query.md) is the per-key law;
alternatives are `⟦k:v₁|v₂⟧ = ⟦k:v₁⟧ ∨ ⟦k:v₂⟧`). The `+`/`-` sign is not
part of the atom — it says how the atom joins its axis.

**Axes.** `axis(t)` is the token's field: one of the six column keys,
`planned`, `ref`, or `text` — `substring:` and free words share `text`.
Group the query's narrowing tokens by axis; on each axis `A` let

```
P = its plain tokens     N = its negated (-) tokens     W = its + tokens
```

**One axis.** With `⋀ ∅ = ⊤` and `⋁ ∅ = ⊥`:

```
base(r) = ⋀{ ⟦t⟧(r) | t ∈ P } ∧ ⋀{ ¬⟦t⟧(r) | t ∈ N }
wide(r) = ⋁{ ⟦t⟧(r) | t ∈ W }

⟦A⟧(r)  = (P ∪ N ≠ ∅  ∧  base(r))  ∨  wide(r)
```

One formula covers the three shapes: no `+` tokens gives `base` alone
(today's law, unchanged); `+` beside plain tokens gives `base ∨ wide`; an
axis of only `+` tokens gives `wide` alone, so a lone `+tag:work` is
`tag:work`.

**The query.** Axes conjoin: `⟦Q⟧(r) = ⋀ { ⟦A⟧(r) | A has a token }`. An
empty query serves every row.

### Laws

1. **Order-independence.** `⟦Q⟧` is invariant under any permutation of Q's
   tokens — grouping is by key, never adjacency.
2. **Conservativity.** A query with no `+` token means exactly what it
   means today.
3. **Widening is per-axis.** If Q already narrows on `axis(t)`, then
   `⟦Q⟧ ⊆ ⟦Q +t⟧` — appending `+t` only adds rows. On a FRESH axis `+t`
   conjoins a new `⟦t⟧` and narrows; `+` widens against its own axis's
   filters, never against the whole query.
4. **Idempotence.** A `+` token repeated changes nothing (`∨` absorbs).
5. **Alternatives agree with `+` on a bare axis.** `k:v₁|v₂ ≡ k:v₁ +k:v₂`
   when the axis holds nothing else. Beside another plain token they part:
   `k:u k:v₁|v₂` is `u ∧ (v₁ ∨ v₂)` where `k:u k:v₁ +k:v₂` is
   `(u ∧ v₁) ∨ v₂`.
6. **Negation stays in the conjunction half.** `-k:v +k:w` is `¬v ∨ w`.

### Derivations

`priority:[#A] tag:book +priority:[#B]`

```
priority: P={[#A]}  W={[#B]}   →  A ∨ B
text/tag: tag axis P={book}    →  book
⟦Q⟧ = (A ∨ B) ∧ book
```

`tag:work tag:home +tag:fun`

```
tag: P={work,home}  W={fun}    →  (work ∧ home) ∨ fun
```

`-state:TODO +state:DONE`

```
state: N={TODO}  W={DONE}      →  ¬TODO ∨ DONE
```

`+priority:[#B]` (fresh axis, nothing else)

```
priority: P∪N=∅  W={[#B]}      →  ⊥ ∨ B  =  B      -- ≡ priority:[#B]
```

### Relational reading

The language is a small relational-algebra fragment, and a query compiles
to ONE dataframe-style pipeline:

```
⟦Q⟧  =  df.filter(⋀ axis-exprs).orderBy(sort:).select(columns:)
```

Each axis compiles to one boolean expression — `(A ∨ B) ∧ book` is
`filter((priority === A || priority === B) && tag(book))`. The append-only
chain `df.filter(p).filter(q)` expresses exactly the `+`-free fragment:
appending a filter can only intersect, which is conservativity (law 2). `+`
is the reason the chain form stops sufficing — it is a per-axis UNION, so
it rewrites its axis's expression rather than appending a new stage (law
3 is the chain-append property holding per axis, lost at the query level).
`ref:` is the one key that is a semi-join underneath, against the target's
spellings rather than the row's own cells. Kept OUT on purpose: joins,
grouping, nesting — the rejected `or:(…)` below — which is what keeps a
query a flat, human-typable URL string.

### Edge cases

| token                | reads as                                                     |
| -------------------- | ------------------------------------------------------------ |
| `+sort:title`        | refused, the `-sort:` 400 shape                              |
| `+k:` (empty value)  | dropped ahead of grouping; adds nothing, establishes no axis |
| `+-x`                | adds the free-text needle `-x` — the first sign stands       |
| `-+x`                | drops the rows carrying `+x` — the second sign is body       |
| `"+state:x"`         | free text (quoted)                                           |
| `+STATE:x`           | added free text (keys are lowercase)                         |
| `+state:A\|B`        | both alternatives join `W` as one atom                       |

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

## As delivered

Four readings were settled against the shape above:

- **A vacuous `+` token adds nothing and establishes no axis.** A `+` whose
  value yields no atoms — `+state:`, `+state:|`, a lone `+` — is dropped
  before axis grouping. Read literally, `(P ∪ N ≠ ∅ ∧ base) ∨ wide` would make
  `+state:` on a fresh axis `⊥` and empty the table, which this proposal's own
  "narrows nothing, adds nothing" and `query.md`'s "a half-typed token never
  empties the table" both forbid. A lone `-` still empties the table; that
  asymmetry is deliberate and stays.
- **Vacuity was widened past the `+` sign** (2026-08-20, after review). The
  rule above holds of ANY token naming no atom, unsigned and added alike: a
  half-typed `state:` or `state:|` is dropped before axis grouping wherever it
  stands. Read off the formula, `state: +state:DONE` leaves `P = {state:}` with
  an empty `base = ⊤`, so `(P ∪ N ≠ ∅ ∧ base) ∨ wide` floods the axis to every
  row — a reader half-types one filter, adds a second, and the table serves
  everything. The drop takes the unsigned and the added; the NEGATED sign keeps
  its inversion law, so a `-` naming no atom still inverts the match-everything
  term and a lone `-` still empties the table.
- **The refusals are spelled per key**, as their `-` twins are: `a sort key
  cannot be added`, `a columns key cannot be added`, `a view key cannot be
  added`. The `an order key cannot be added` above was the shape, never the
  letter. `+view:NAME` is a 400 of its own; `-view:NAME` is left as it is,
  conservativity forbidding a change to any `+`-free query.
- **The parity vectors are unchanged.** `fixtures/parity/filter-query.json`
  exists neither in this repo nor in `../table-view` nor anywhere in git
  history, so the `+` case named in the sketch has nothing to join.

The user docs are `docs/query.md`'s "Adding: `+` widens its own axis" and one
row in the README's query crib.
