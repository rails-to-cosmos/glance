# Proposal — datetime comparisons in the flat grammar

**Status:** proposed · **Date:** 2026-08-21 · **Origin:** user — *"the DSL
should support datetime comparisons — for SCHEDULED, DEADLINE, CLOSED, and
custom properties where org-mode timestamps are used (active and inactive
forms)."*

The ask names the DSL, and the DSL cannot say it yet. The flat `?q=` string is
the one truth ([the typed DSL behind the `.`
door](2026-08-21-the-typed-dsl-behind-the-dot-door.md#the-language-defined) —
*"the surface can spell nothing the flat string cannot"*), and that proposal's
own evolution rule says where the operator has to land first: *"`=~`, `<`, `>`
are the obvious next ones and none may land before the flat grammar has the
predicate underneath"*
([L7](2026-08-21-the-typed-dsl-behind-the-dot-door.md#l7--evolution)). So the ask
is two designs. **This one is the flat half and it ships alone;** the typed half
is the amended
[L8](2026-08-21-the-typed-dsl-behind-the-dot-door.md#l8--datetime-comparisons),
which cannot ship before it.

## The law in one line

A timestamp-valued key takes a COMPARISON in the value position, and the bare
value keeps its prefix reading — which is that same comparison said as a closed
interval.

```
deadline:>=2026-09-01 deadline:<2026-10-01
```

serves the rows due in September. The two tokens AND on one axis, which is the
axis law's own conjunction ([additive-filters](../done/2026-08-20-additive-filters.md):106-113),
so the range needs no grammar of its own. Today that same set is spelled
`deadline:2026-09` — and only because September is a whole calendar month. A
range no prefix names cannot be spelled at all today, and neither can "before"
or "after".

## Why the spelling is free: today's `scheduled:>2026-09` is dead ground

The four operator spellings are picked because the flat grammar already carries
them and answers nothing:

- `scheduled:` and `deadline:` resolve as keys (`Filter.hs`:222 through
  `filterKeys`, `Query.hs`:1935-1936), so `scheduled:>2026-09` is a PREDICATE
  and never free text.
- `splitKey` breaks on the first `:` or `=` alone (`Filter.hs`:192-196), so the
  key is `scheduled` and the value is the whole `>2026-09`, operator included.
  Neither `<` nor `>` is a separator, and adding one would be a different
  proposal (see **Alternatives**).
- The matcher's date arm is `T.isPrefixOf value . cell` (`Filter.hs`:341, armed
  by `prefixed` at :344 over `dateKeys` and `plannedKey`), and the cell is
  `isoStamp`'s output — `%Y-%m-%d` or `%Y-%m-%d %H:%M` (`Query.hs`:1101-1105),
  which opens with a digit and never with a bracket.

So no cell can start with `>`, `<`, `>=` or `<=`, and **every comparison-shaped
token in the language today serves exactly zero rows.** The renderer agrees for
a different reason and reaches the same answer: its date arm is
`cells[i].startsWith(v)` (`assets/table-view.js`:2218) and its fallback for a
column it did not sample as dated is `includes(v)` (:2219) — a substring search
for the characters `>2026-09`, which no ISO cell carries either.

The one class of query whose meaning changes is therefore the class that today
serves nothing, or — under `-` — everything. The conservativity table below is
exhaustive.

## Grammar

- A comparison lives INSIDE the value, at its head:
  `key:OP DATE` with no space — `scheduled:>=2026-09`.
- `OP` is one of `>=`, `<=`, `>`, `<`, read LONGEST FIRST so `>=` is never `>`
  followed by a literal `=`.
- **The operator is read on the timestamp-valued keys and nowhere else.**
  `scheduled:`, `deadline:`, `planned:` today; `closed:` when it lands; a
  property key in phase 3. On every other key the character is body text, so
  `title:>x` is the substring it is today and `tag:<a>` is the tag cell search
  it is today.
- **The operator is read PER ALTERNATIVE.** `predTest` splits on `|` before it
  reads a value (`Filter.hs`:300), so `scheduled:>=2026-08|<2026-07` is two
  atoms ORed, each with its own operator. A `|` is never a range.
- The signs are untouched and ride outside the value, as they always have:
  `-deadline:<2026-09` negates the whole token and `+deadline:<2026-09` widens
  the deadline axis.
- `=` stays `:`'s alias, and it splits FIRST: `scheduled=>=2026-09` is the same
  token as `scheduled:>=2026-09`. `scheduled>=2026-09` with no separator is free
  text — `splitKey` would break at the `=` and leave the key `scheduled>`, which
  no `fieldOf` resolves (`Filter.hs`:217-222).

### The date literal

A date literal is what the bare form already accepts: **any non-empty prefix of
an ISO stamp**, and it names the INTERVAL of every stamp that prefix reaches.
`2026-08` is a month, `2026-08-0` is the first nine days (pinned today at
`test/TestFilter.hs`:289), `2026-08-03 09` is an hour.

**The granularity law, in one sentence: `<` and `>=` cut at the interval's FIRST
instant, `<=` and `>` cut at its LAST.** So `deadline:<2026-09` is "before
September" and `deadline:<=2026-09` is "September or earlier", each the reading
the operator's own word has in English, and each agreeing with the bare form
(below, law 3).

A literal that does not open with a digit is no date: it matches no row, the way
`state:TOD` matches no row (`docs/query.md`:53). An operator with NO literal —
`scheduled:>`, `scheduled:>=` — is the half-typed token and narrows nothing.

## Formal semantics

Notation: `c` is the row's stamp for the key as ISO text, `⊥` the empty cell,
`D` the date literal, `⊑` "is a prefix of", and `<` byte order on the text.

### The atoms

```
⟦k:D⟧    (r) =  c ≠ ⊥  ∧  D ⊑ c            -- today's prefix arm, unchanged
⟦k:<D⟧   (r) =  c ≠ ⊥  ∧  c < D
⟦k:>=D⟧  (r) =  c ≠ ⊥  ∧  c ≥ D
⟦k:<=D⟧  (r) =  c ≠ ⊥  ∧  (c < D ∨ D ⊑ c)
⟦k:>D⟧   (r) =  c ≠ ⊥  ∧  (c > D ∧ D ⋢ c)
```

Two things are worth reading off the shape. **The last two need no date
arithmetic** — no successor month, no leap year, no calendar at all: the
interval's last instant is spelled as "everything the prefix reaches", which is
the prefix test the matcher already runs. And **`c ≠ ⊥` is a conjunct in every
line**, which is law 5 and is the one guard the implementation must not forget:
`"" < "2026-09"` is true in byte order, so an unguarded `<` would serve every
undated row.

Where a key names SEVERAL cells — `planned:` names both date columns
(`Filter.hs`:78-79, :226) — the atom is asked of each and ORed, unchanged
(`cellsTest`, :331-335). `⟦planned:<D⟧` is "the schedule is before D, or the
deadline is".

### Laws

1. **Trichotomy.** For a dated row exactly one of `k:<D`, `k:D`, `k:>D` holds.
   Byte order is total, and `D ⊑ c` implies `c ≥ D`.
2. **The inclusives are the unions.** `k:<=D ≡ k:<D|D` and `k:>=D ≡ k:D|>D`.
3. **The bare form is the closed interval.** On a single-cell key,
   `k:D ≡ k:>=D k:<=D`. Derivation: `(c ≥ D) ∧ (c < D ∨ D ⊑ c)` = `(c ≥ D) ∧
   D ⊑ c` = `D ⊑ c`, since a prefix is never below its literal. The comparison
   forms and the prefix form are ONE reading of one literal.
4. **Byte order is time order.** ISO stamps are zero-padded and fixed-width per
   granularity (`isoStamp`, `Query.hs`:1101-1105), so `c < c'` iff `c` is the
   earlier stamp; and an untimed stamp sorts before the same day's timed ones,
   which is that day's first instant. The sort chain already reads these cells
   this way — `by CSched = ByFolded` (`AGENTS.hs`:2583).
5. **The empty cell is outside every comparison.** `*empty*` stays the only name
   for it (`docs/query.md`:226), and `-k:<D` therefore serves the undated rows
   too. The precedent is pinned: `-planned:2026-08` serves the undated rows
   today (`test/TestFilter.hs`:305).
6. **Negation is no mirror.** `-k:<D ≢ k:>=D`. The first serves the undated rows
   and the second does not, by law 5. The four operators do not pair off under
   the sign, and the surface must never rewrite one into another.
7. **The per-axis law is untouched.** A comparison is one more ATOM KIND. It is
   read below the `|` split, which is below the sign, which is below the axis
   grouping — so [additive-filters](../done/2026-08-20-additive-filters.md)'s
   `⟦A⟧(r) = (P ∪ N ≠ ∅ ∧ base(r)) ∨ wide(r)` (:106-113) is quoted, not amended.
   Only `⟦t⟧`, the atomic predicate that formula is parametric in, gains cases.
8. **The range is the axis conjunction.** `k:>=A k:<B` on a single-cell key is
   `A ≤ c < B`, because two plain tokens on one axis AND (`docs/query.md`:314-316).
   No new form is owed. The exception is law 9.
9. **`planned:` cannot say a range.** `planned:>=A planned:<B` is
   `(s ≥ A ∨ d ≥ A) ∧ (s < B ∨ d < B)`, which is NOT
   `(A ≤ s < B) ∨ (A ≤ d < B)`. A row scheduled 2027-01-01 with a deadline of
   2020-01-01 passes both tokens for A = 2026-08-01, B = 2026-09-01 and lies in
   neither range. **No pair of tokens says "one date cell inside the range" on a
   multi-cell key.** This is the range's parting case, the same shape as
   additive-filters' law 5, and it is named as a theorem so the hole is a stated
   fact rather than a caveat. `A..B` is the reserved form that would close it;
   see the recommendation below.

### Derivations

`deadline:>=2026-09-01 deadline:<2026-10-01`

```
deadline: P = {>=2026-09-01, <2026-10-01}   W = ∅
        →  (c ≥ "2026-09-01") ∧ (c < "2026-10-01")     -- September
```

`scheduled:<2026-09 +scheduled:*empty*`

```
scheduled: P = {<2026-09}   W = {*empty*}
        →  (c ≠ ⊥ ∧ c < "2026-09")  ∨  c = ⊥           -- scheduled before September, or not at all
```

`-planned:>=2026-09`

```
planned: N = {>=2026-09}
        →  ¬( (s ≠ ⊥ ∧ s ≥ D) ∨ (d ≠ ⊥ ∧ d ≥ D) )      -- undated rows included, law 5
```

### Worked table

Against a row set spelling `scheduled`, `deadline` and (phase 2) `closed`:

| query | serves |
|---|---|
| `deadline:<2026-09` | due before September; undated rows excluded |
| `deadline:<=2026-09` | due in September or earlier |
| `deadline:>2026-09` | due after September |
| `deadline:>=2026-09` | due in September or later |
| `deadline:2026-09` | due in September — ≡ `deadline:>=2026-09 deadline:<=2026-09` |
| `deadline:>=2026-09-01 deadline:<2026-10-01` | the same September, spelled as a range |
| `deadline:>=2026-09-15 deadline:<2026-10-08` | a range no prefix names |
| `-deadline:<2026-09` | not due before September — the undated rows among them |
| `scheduled:<2026-09 deadline:>2026-09` | scheduled early, due late |
| `planned:<2026-09` | either date cell is before September |
| `scheduled:>=2026-08\|<2026-07` | an OR of two comparisons, never a range |
| `closed:>=2026-08-01` | closed since August (phase 2) |
| `deadline:<2026-09 +deadline:*empty*` | due before September, plus the undated |

## The range spelling: two tokens, and the one thing they cannot say

**Recommendation: no range atom. Two tokens are the range, and `A..B` is
reserved rather than spent.**

Three grounds, in order of weight:

1. **The axis law already ANDs.** `k:>=A k:<B` is the conjunction by
   `docs/query.md`:314-316, spelled with grammar that exists. A sugar whose
   desugaring is "write the two tokens you already can" earns nothing.
2. **The prefix form already gives every calendar-aligned range** — a year, a
   month, a day, an hour — and those are the ranges a reader asks for. What is
   left for `A..B` is the ranges no prefix names, which two tokens say.
3. **It is not free.** `..` inside a value competes with a literal `..`, needs
   its own half-typed reading (`k:2026-08..`), its own interaction with `|`, and
   its own row in every readers' table. Two tokens need none of that.

**The cost, stated: law 9.** On `planned:` — and on any future multi-cell key —
`A..B` would say something two tokens cannot, because a single ATOM is asked of
each cell in turn where two TOKENS are ANDed at the axis. That is the whole
argument for the form, it is one key wide, and the workaround is exact: name the
cell you meant (`scheduled:>=A scheduled:<B`) instead of asking `planned:`. If
the multi-cell range is ever wanted, `A..B` lands as one more atom kind under
the same law 7 and breaks nothing here.

## `closed:` — the seventh axis

**Recommendation: `closed:` becomes a narrowing key of its own, reading the
planning line's `CLOSED:` stamp as ISO, and `planned:` is untouched.**

### The value is already read

`CLOSED:` is parsed, spanned and displayed today:

- `hsClosed :: !(Maybe Span)  -- ^ the CLOSED: timestamp alone, keyword
  excluded` (`src/Data/Org/Types.hs`:225), filled at `src/Data/Org/Parser.hs`:126.
- `customCell` serves it as a COLUMN — `wanted == "closed" = sliceSpan (hrDoc r)
  <$> hsClosed (headlineSpans r)` (`Query.hs`:1924-1930), which
  `docs/query.md`:270-273 documents as the one custom column that is not a
  drawer key.
- The model names it as a closed sum: `data CustomSrc = PlanClosed | FromDrawer`
  (`AGENTS.hs`:2689-2693).

So nothing has to be parsed that is not parsed. What is missing is a KEY.

### It cannot be a column key

`filterKeys` is derived from `viewColumns` (`Query.hs`:1935-1936, :1896-1904),
the ONE list four readers agree through (`docs/invariants.md`:148-153). Adding
`closed` there would add a seventh column to the default view, which is not the
ask. So `closed` joins the grammar's OWN keys beside `planned`, `ref` and
`substring` — a `Field` constructor, a `fieldOf` equation, and its own arm in
`keyTest`, since the value is not one of the six cells in `hrSearch`
(`viewCells`, `Query.hs`:1932-1933).

### The cell it reads, and the brackets

The two existing date keys read `isoStamp` output — no brackets, no day name
(`Query.hs`:401-402, :1101-1105). The `closed` COLUMN's cell is the span
verbatim: `[2024-01-02 Tue 10:00]`, brackets and weekday included
(`test/TestSpans.hs`:244).

**Recommendation: the KEY reads the ISO stamp and the COLUMN keeps its raw
display.** One record field, `hrClosed = isoStamp <$> closed h`, beside the two
that already exist. Then `closed:` reads exactly as `scheduled:` and
`deadline:` read, every law above applies unchanged, and no reader learns a
second date shape.

The divergence this creates is named rather than hidden: **the matcher reads
through org's brackets where the display wears them**, which is the figure
`priority:` already has — *"Matching reads THROUGH org's brackets:
`priority:A` = `priority:[#A]`"* (`Filter.hs`:339-340, `docs/query.md`:54). A
reader who filters `closed:2026-08` and shows `columns:closed` sees
`[2026-08-03 Mon]` in a cell the key matched as `2026-08-03`.

### `planned:` keeps its exclusion

`planned:` is SCHEDULED-or-DEADLINE and stays so: `dateKeys = ["scheduled",
"deadline"]` (`Filter.hs`:43-44) feeds `dateColumns` (:78-79) feeds
`fieldCells Planned` (:226), and `docs/query.md`:65-67 says it in words —
*"a row is planned when either SCHEDULED or DEADLINE holds anything (`CLOSED:`
does not count)"*. The fixture pins it: the `Drop` row carries a `CLOSED:` and
sits in `planned:*empty*` (`test/TestFilter.hs`:280-281).

**`closed:` is its own axis and joins no other.** The consequence is worth
stating: `planned:2026-08 closed:2026-08` is the conjunction (planned AND
closed in August), and the UNION of two axes has no spelling — the same gap
additive-filters names when it defers a general `or:(…)` combinator (:225-227).
Widening `planned:` to include CLOSED was considered and rejected: it would
change what every existing `planned:` query serves, which law 2 of the
conservativity table forbids.

## Custom timestamp properties — the honest scoping

**Recommendation: phase the ask. The planning keys ship first and alone; custom
properties are a second design that needs a KEY SPELLING before it needs a
comparison.**

### No flat key reaches a property today, and that is load-bearing

`fieldOf` resolves `planned`, `ref`, `substring`, the three view keys, and
`elemIndex key filterKeys` — a CLOSED list (`Filter.hs`:217-222). The closure is
pinned on purpose, by a test that spells the roster rather than deriving it:

> `test/TestFilter.hs`:307-329 — *"no tree can take the key away, the keys being
> a closed list"*, asserting the ten keys literally and asserting that no tag the
> fixture carries resolves as one.

`columns:owner` reaches a drawer key (`docs/query.md`:270-273, `customCell`
`Query.hs`:1924-1930) — but that is the COLUMN set, which is open by design, and
it narrows nothing. **There is no flat filter key today that names a property.**

The renderer is the exception and it is a standing divergence: its key set is
the VIEW's columns — `columnKeys = () => columns().map(c => c.key)`
(`assets/table-view.js`:2135, :2143-2149) — so under `columns:closed` a
`closed:` token is a live predicate on the page and free text on the server. The
proposal's phase 2 closes exactly that gap for `closed`; phase 3 closes it for
the rest.

### Phase 3, spelled

- **The key spelling is a NAMESPACE, and it is required rather than preferred.**
  `prop.NAME:` — `prop.due:>=2026-09`. `splitKey` breaks on the first `:`, so the
  key arrives whole as `prop.due` and `fieldOf` gains ONE prefix rule. Letting a
  bare property name be a key would open the key set and break the pinned law
  above: a tree defining a `:state:` or `:ref:` property would silently take a
  key away. A namespace keeps the closure — the prefix is closed even though the
  name behind it is open — so `TestFilter.hs`:307-329 keeps meaning what it
  means.
- **Rejected: `prop:NAME:VALUE`,** the property name smuggled into the value.
  That makes a value into a mini-grammar, which is the reading the DSL proposal
  already rejects for `["Deadline:desc"]`
  ([the direction spelling](2026-08-21-the-typed-dsl-behind-the-dot-door.md#sort-and-the-direction-spelling)).
- **The cell is the drawer value verbatim** (`customCell`, `Query.hs`:1927-1929),
  so it wears whatever org wrote — including the brackets.

### Active and inactive, answered

Org writes two timestamp kinds and the parser keeps both: `data TimestampStatus
= TimestampActive | TimestampInactive` (`src/Data/Org/Types.hs`:508-513), with
the brackets DERIVED from that sum rather than spelled twice — `activeBrackets`
/ `inactiveBrackets` off `bracketsOf` (`Query.hs`:1572-1581). An active stamp is
`<2026-09-01 Tue>`, an inactive one `[2026-09-01 Tue]`.

- **Storage.** In a property cell, verbatim — brackets, weekday, time and any
  repeater as the author typed them. Nothing normalizes a drawer value.
- **Does the comparison fold the brackets?** **Yes, the way `priority:` folds
  `[#A]`.** A timestamp cell is read through its brackets and its weekday, and
  the display keeps them. One folding function serves the property cells and the
  `closed` COLUMN alike, and the kind — active or inactive — does not change what
  a date compares as.
- **Is a kind meta owed?** **No, and the model says why.** `metaHome :: Meta ->
  MetaHome` is a FUNCTION, one home per meta (`AGENTS.hs`:2523-2534), and
  `*active*` / `*inactive*` are already `state:`'s with a wholly different
  meaning (`docs/query.md`:227-228). Reusing the two words on a property key
  would make `metaHome` a relation and make one word mean two things — the
  failure the DSL's roster law is built to prevent
  ([§0](2026-08-21-the-typed-dsl-behind-the-dot-door.md#0--the-roster-law-which-decides-every-spelling)).
  A kind predicate,
  if ever shown to be owed, is TWO NEW constructors on the `Meta` sum with words
  of their own, named by the compiler under the closed-sum discipline
  (`docs/invariants.md`:155-159). Phase 3 ships without one and says so.
- **Dateness is decided by the CELL, never by the key.** `prop.due:` cannot know
  its cell is a timestamp, so a comparison reads the cell as one and fails where
  it is not. That is the per-cell twin of the renderer's sampled `dateColumn`
  (`assets/table-view.js`:2182-2197), and it widens the `DateNess` gap
  (`AGENTS.hs`:2707) rather than inventing a new mechanism.

## The renderer, term for term

The renderer is a port term for term (`Filter.hs`:1-2), and the last proposal's
sketch is the reason this section is long. Additive filters named ONE divergence
row for the renderer and shipped; the row was not the whole story, and the miss
is filed: [the renderer reads the added sign as
text](../../bugs/fixed/2026-08-20-the-renderer-reads-the-added-sign-as-text.md)
— *"the proposal's implementation sketch called that gap row the whole renderer
story; completion and the strip are the part it missed, where the sign is
rewritten into text the reader never typed"* (:49-54). The AddKey history closed
the other way: the sign landed in all four surfaces, and the model now records
that *"THE RENDERER CARRIES THE ADDED SIGN TERM FOR TERM, so the divergence
table names no added key"* (`AGENTS.hs`:2818-2827).

**A comparison lands in all four surfaces or it is a bug with a proposal in
front of it.** Enumerated:

| surface | today | what a comparison owes it |
|---|---|---|
| the scanner | `scanQuery`/`scanQ` split sign, quotes and body (`Filter.hs`:120-137) | NOTHING. The operator is inside the value and never a separator — the one surface this proposal does not touch |
| the matcher | `cellTest`'s date arm, `cells[i].startsWith(v)` (`assets/table-view.js`:2218) | the four operators, guarded on a non-empty cell, behind the same `dateColumn(i)` verdict |
| completion | `stageAt` opens the value stage on `t.value` past the last `\|` (`:3796-3824`) | open the value stage BEHIND the operator, so `scheduled:>=2026-0` completes dates and not literals; offer the four operators over an empty date value |
| the chip strip | `spelled` prints key and value (`:3193`) | print the token as written — the operator is value text, so this is a case to PIN rather than a change to write |

**And one gap widens, so it is named rather than left silent.** The renderer
decides dateness by SAMPLING — `DATEISH` and `COULD_BE_DATE` weighed by
`sampledShape` over at most 40 cells needing 2 in shape and 0 contrary
(`assets/table-view.js`:2181-2197, `AGENTS.hs`:2713-2714) — where the producer
names two keys outright. That is the standing `DateNess` gap, `Neither`
(`AGENTS.hs`:2707), and it is already noted as *"under two dated rows the
renderer finds no date column, so `scheduled:` substring-matches there"*
(:2765-2767). Under a comparison the same page turns a live predicate into a
substring search for `>=2026-09`, which matches nothing: the renderer goes
strictly NARROWER, which flips `DateNess` from `Neither` to a stated direction
for comparison values. Phase 1 updates that row's comment; it does not add a
constructor.

`closed:` DOES add one. A page carrying no `closed` column cannot decide the
key at all — the same shape as `RefKey`, *"undecidable off a page, so it reads
as free text"* (`AGENTS.hs`:2708) — so `gaps` gains `(ClosedKey, Renderer)` in
phase 2, and where `columns:closed` IS shown, the renderer's arm reads a
BRACKETED cell and must fold it.

## What breaks nothing

Conservativity, exhaustively. "Today" is read off `Filter.hs`:341 and :344.

| query shape | today | after | why |
|---|---|---|---|
| every query with no `<`/`>` at the head of a timestamp key's value | — | **byte-identical** | the operator is read at one position on one set of keys |
| `title:>x`, `tag:<a>`, `state:>A`, `priority:>A` | substring / whole-value | **unchanged** | the operator is read on the timestamp keys alone |
| `substring:>x`, bare `>x`, `"scheduled:>=2026-09"` | free text | **unchanged** | free text has no operator, and a leading quote is free text whole (`docs/query.md`:21) |
| `sort:…`, `columns:…`, `view:…` | shaping | **unchanged** | comparisons are narrowing-only, and no refusal moves |
| `scheduled:>2026-09`, `deadline:<=2026-09`, `planned:>=2026-09` | serve **nothing** | serve the compared rows | the dead ground this proposal spends |
| `-scheduled:>2026-09` | serves **every** row | serves the rows not after, undated among them | the negation of the above, laws 5 and 6 |
| `+scheduled:>2026-09` alone on its axis | serves **nothing** | serves the compared rows | `wide` was a false atom |
| `scheduled:>`, `scheduled:>=` | serve **nothing** | narrow **nothing** | the half-typed law, `docs/query.md`:60-61 — a change in the direction the law already points |
| `closed:2026-08` | free text: the literal string, so nothing | the CLOSED axis | phase 2 |
| `prop.due:2026-08` | free text: the literal string, so nothing | the property axis | phase 3, and the one conservativity cost of the namespace |

**Every changed cell above is a query that today serves nothing or everything.**
No query that serves a proper subset of the rows changes what it serves.

## Edge cases

| token | reads as |
|---|---|
| `scheduled:>` | vacuous: narrows nothing, establishes no axis (`vacuous`, `Filter.hs`:269-270) |
| `-scheduled:>=` | empties the table — the negated vacuum's own law, as `-state:` is today |
| `scheduled:>banana` | a literal opening with no digit: matches no row, `state:TOD`'s reading |
| `scheduled:>*empty*` | a meta is no date literal: matches no row |
| `scheduled:>=2026-0` | a legal prefix literal — the first nine days of a month (`TestFilter.hs`:289) |
| `scheduled:">=2026-09"` | the same comparison: the quote does not OPEN the token, so it strips mid-value |
| `"scheduled:>=2026-09"` | free text whole — the leading quote |
| `scheduled=>=2026-09` | the same comparison: `=` is `:`'s alias and splits first |
| `scheduled>=2026-09` | free text: the key would be `scheduled>`, which resolves to nothing |
| `scheduled:>=` `2026-09` (a space) | two tokens: a vacuous comparison and the free-text needle `2026-09` |
| `scheduled:>=A\|<B` | two atoms ORed, each with its own operator |
| `scheduled:>=A scheduled:<B` | the range: two plain tokens ANDing on one axis |
| `planned:>=A planned:<B` | NOT the range — law 9 |
| `deadline:<2026-09 +deadline:*empty*` | the comparison widened by a meta; both are atoms |
| a row-range stamp `<a>--<b>` | compares by its START: `isoStamp` reads `tsStart` (`Query.hs`:1102-1103) |
| a repeater `<2026-09-01 Tue +1w>` | compares by the base date, for the same reason |

## Implementation sketch

Three phases, each landing green on `make test` and `make browser-check`, each
shippable alone.

### Phase 1 — the comparison, on `scheduled`, `deadline`, `planned`

- **`AGENTS.hs`.** A closed sum `data Cmp = CLt | CLe | CGe | CGt` with one
  equation per constructor and no wildcard (`docs/invariants.md`:155-159); a
  reader `cmpOf :: String -> Maybe (Cmp, String)` splitting the operator off the
  value longest-first; the `MPrefix` arm of `matchOf` (`AGENTS.hs`:2365-2374)
  reads through it. `Match` gains no constructor — the comparison is read off the
  VALUE, and matching stays *"by KEY NAME and never by the declared kind"*
  (:2358). `queryNotes` gains the granularity law, the empty-cell law and law 6.
- **`Filter.hs`.** `cellsTest`'s `prefixed` arm (:341, :344) becomes one
  `stampTest` over the five atom readings above. `atoms` (:274-277) drops an
  atom that is an operator with no literal, so `vacuous` (:269-270) covers
  `scheduled:>` with no second rule. `valueFor (Col _)`'s fold (:282) is
  harmless over digits and operators and is left alone.
- **The renderer** (upstream in `../table-view/web/table-view.js`, arriving by
  `make sync-renderer`): the four surfaces in the table above.
- **Docs.** `docs/query.md`'s six-key table gains the operator forms on its two
  date rows; a "Comparisons" subsection carries the granularity law, the
  empty-cell law and the range; the metas table is untouched.
- **Tests.** `TestFilter.hs` gains a `comparisonSpec`: the trichotomy, the
  bare-form identity (law 3), the undated row outside every comparison (law 5),
  the `-k:<D` ≢ `k:>=D` pair (law 6), the half-typed operator, the non-digit
  literal, the two-token range, and law 9's counterexample on `planned:` — which
  must go RED if the range is ever desugared into two tokens. One browser case
  beside them; `make interop` unchanged.
- **Parity.** `fixtures/parity/filter-query.json` is named by `AGENTS.hs`:2878
  and DOES NOT EXIST — additive-filters found it *"neither in this repo nor in
  `../table-view` nor anywhere in git history"* (:254-256). So there is nothing
  to add there, and the shared-half story is
  [the parity vectors glance never runs](2026-08-15-the-parity-vectors-glance-never-runs.md),
  which this proposal does not block and would benefit from.
- **Risk: low.** One arm of one function on each side, and the ground it lands
  on is dead.

### Phase 2 — `closed:`, the seventh axis

- **`Query.hs`.** `hrClosed :: !(Maybe Text)` on the record, `isoStamp <$>
  closed h` beside :401-402, forced in `forceRecord`'s `optional` (:1120).
  `viewColumns` and `filterKeys` are UNTOUCHED, so the default view grows no
  column and `customCell`'s `closed` column keeps its raw display (:1926).
- **`Filter.hs`.** `Field` gains `Closed`; `fieldOf`, `fieldCells`, `narrows`,
  `valueFor` and `keyTest` each gain their equation — the compiler names every
  one of them, which is why the sums have no wildcard. `keyTest`'s new arm reads
  `hrClosed` rather than a cell index, so `cellsTest` is unchanged.
- **`AGENTS.hs`.** `Field` gains `FClosed` with the same total-function edits;
  `gaps` gains `(ClosedKey, Renderer)`.
- **Docs and tests.** `docs/query.md`'s "Three more predicates" becomes four, and
  the `planned:` sentence gains "…and `closed:` is its own axis". The key roster
  pinned at `TestFilter.hs`:321-324 gains `"closed"` — the test whose whole job
  is to go red here. The `Drop` fixture row (`TestFilter.hs`:280) becomes the
  positive case for `closed:` and stays the negative one for `planned:`.
- **Risk: low-medium.** The sharp edge is the two readings of one word — the KEY
  reads ISO, the COLUMN displays the bracketed span — and it is pinned by one
  test asserting both off the same row.

### Phase 3 — custom timestamp properties

- `fieldOf` gains the `prop.` namespace rule and a `Field` constructor carrying
  the folded property name; the matcher reads `customCell`'s drawer value
  (`Query.hs`:1924-1930) through the bracket fold.
- The `TestFilter.hs`:307-329 closure test gains the namespace's own case: a
  tree defining a property called `state` is reachable as `prop.state:` and
  still does not take `state:` away.
- The renderer's own key set already admits a shown custom column
  (`assets/table-view.js`:2135, :2143-2149); phase 3 is where that stops being a
  silent divergence and becomes the same key on both sides.
- **Risk: medium**, and it is the phase this proposal recommends reviewing on
  its own evidence rather than on this sketch.

## Alternatives considered

- **A range atom `A..B` now.** Deferred, with law 9 named as exactly what it
  would buy. It does not block later: one more atom kind under law 7.
- **The operator BEFORE the separator** — `scheduled<2026-09`. Rejected:
  `splitKey` would have to break on `<` and `>` as well as `:` and `=`
  (`Filter.hs`:192-196), which turns today's free text `a<b` into a predicate
  and breaks conservativity for every reader who ever typed an arrow in a title
  search.
- **Refusing a malformed literal with a 400.** Rejected: only the shaping keys
  refuse, and *"everything else that fails to parse is free text; everything
  half-typed narrows nothing"* (`docs/query.md`:320-326). A narrowing token has
  never refused and this one does not start.
- **Relative literals — `today`, `+1w`.** The vocabulary exists:
  `planningTimestamp` already reads `today`, `tomorrow` and org's whole offset
  charset on the WRITE path (`Query.hs`:1537-1559). Rejected here because a
  query's meaning would then depend on the clock, and a `?q=` string is meant to
  be shareable, pinnable into a saved view and reproducible in a test. It is a
  proposal of its own, and it composes with this one — the operator would take a
  resolved literal.
- **Driving the comparison off the column's declared `type`.** Rejected against
  the model's own heading, *"Matching, by KEY NAME and never by the declared
  kind"* (`AGENTS.hs`:2363). A wire field would also have to declare a kind per
  custom column, which is the same growth
  [§8 of the DSL proposal](2026-08-21-the-typed-dsl-behind-the-dot-door.md#8-the-roster-on-the-wire)
  rejects, against the descriptor pin at `test/TestSpec.hs`:984.
- **Widening `planned:` to include CLOSED.** Rejected: it changes what every
  existing `planned:` query serves, against `docs/query.md`:65-67 and the
  fixture pinned at `TestFilter.hs`:280-281.
- **Reusing `*active*` / `*inactive*` for the timestamp kind.** Rejected:
  `metaHome` is a function from a meta to ONE home (`AGENTS.hs`:2523-2534), and
  the two words are `state:`'s.

## As delivered (phase 1)

Phase 1 shipped on `scheduled:`, `deadline:` and `planned:`. Two readings go
past the sketch above — the range and `*today*`; the rest landed as written.

- **The four operators, as the formal semantics spell them.** `>=`, `<=`, `>`,
  `<` at the head of the value, read longest first, on the three timestamp keys
  and nowhere else. The granularity law, the empty-cell guard (law 5) and
  "negation is no mirror" (law 6) are the three the tests pin, and the
  conservativity table held: every query whose meaning moved is one that served
  nothing, or — under `-` — everything.

- **`A..B` LANDED, against the recommendation above.** The range section
  recommends "no range atom … `A..B` is reserved rather than spent", and law 9
  is why it was spent instead: on `planned:` a range says ONE DATE CELL INSIDE
  THE INTERVAL, which no pair of tokens says, and `planned:` is the key an
  agenda is written on. On a single-cell key `A..B` ≡ `>=A` and `<=B` on the
  axis, so the two spellings agree wherever two tokens can speak at all, and
  law 9's counterexample is where they part. The three grounds the section
  gives against the form are costs paid rather than arguments withdrawn.

- **`*today*` is new, and the user asked for it.** Alternatives rejects
  relative literals — *"a query's meaning would then depend on the clock, and a
  `?q=` string is meant to be shareable, pinnable into a saved view and
  reproducible in a test"* — and names the composition it would take: *"the
  operator would take a resolved literal"*. That is what shipped. `*today*` is
  a DATE VALUE wearing the starred family's shape, legal wherever a date
  literal stands: bare (`scheduled:*today*` is the exact day, by the prefix
  reading), behind any operator, at either end of a range. It resolves ONCE per
  request at filter compile — the one-clock-read invariant
  (`docs/invariants.md`:118-120) — against the server's local day, as
  `YYYY-MM-DD`. The clock is read once and written down before any row is
  asked; what a saved view pins is the WORD, so `view:agenda` means the day it
  is applied on. The renderer answers the same word against the BROWSER's local
  day: one machine over loopback, so the skew is a midnight the request
  straddles, and it is noted rather than closed.

- **The renderer is term for term**, the AddKey lesson taken: the matcher's
  four operators, the range and `*today*` behind the same `dateColumn` verdict;
  completion opening the value stage BEHIND the operator; the chip strip
  printing the token as written, which is a case to pin rather than a change to
  write.

- **`closed:` and `prop.NAME:` stay phased.** Phase 2 and phase 3 are unbuilt:
  `closed` is free text on the flat side, a property is reachable only as a
  COLUMN, and the key roster pinned at `test/TestFilter.hs`:307-329 is the ten
  it was. The status line above stays **proposed** until they close.

- **`*today*` costs the typed DSL a decision it has not made.**
  [L8](2026-08-21-the-typed-dsl-behind-the-dot-door.md#l8--datetime-comparisons)
  rule 2 reads *"The right side is one `Date`. No list, no `Any`, no `All`, no
  `Meta`"* — and `*today*` is a `Meta` standing in exactly that position on the
  flat side. So either `Today` joins the prelude as the one constructor legal
  where rule 2 admits none, or the typed surface reaches the day some other
  way, or it cannot spell a query the flat string can. The roster law
  ([§0](2026-08-21-the-typed-dsl-behind-the-dot-door.md#0--the-roster-law-which-decides-every-spelling))
  makes the word the LANGUAGE's, so the question is which shape it wears rather
  than whether it exists. **Flagged, not decided**, and noted at L8 rule 2.

The user docs are
[`docs/query.md`'s "Comparisons on the date keys"](../../query.md#comparisons-on-the-date-keys).

## See also

- [the typed DSL behind the `.` door](2026-08-21-the-typed-dsl-behind-the-dot-door.md)
  — the surface this proposal is the prerequisite for; its
  [L8](2026-08-21-the-typed-dsl-behind-the-dot-door.md#l8--datetime-comparisons)
  is the typed half and depends on this file.
- [additive-filters](../done/2026-08-20-additive-filters.md) — the per-axis law
  this proposal quotes rather than amends (:106-113), and law 5's parting case
  that law 9 rhymes with.
- [the renderer reads the added sign as text](../../bugs/fixed/2026-08-20-the-renderer-reads-the-added-sign-as-text.md)
  — the four-surface lesson the renderer section is written against.
- [the parity vectors glance never runs](2026-08-15-the-parity-vectors-glance-never-runs.md)
  — where the shared half of the grammar would be checked, if it were.
- [`docs/query.md`](../../query.md) — the whole law of the string being amended.
- [`docs/invariants.md`](../../invariants.md) — the closed-sum and one-list rules
  every phase above is spelled under.

Phases 2 and 3 are inert until reviewed.
