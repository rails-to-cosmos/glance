# The query language

One string states the narrowing, the order, the columns and the view. It is
[table-view](https://github.com/rails-to-cosmos/table-view)'s grammar, ported
term for term; the README's table is the crib, this page is the whole law.
The model behind it is `AGENTS.hs` ("Query language"); the pinned behaviors
live in `test/TestFilter.hs`.

```
state:*active* -tag:chore "release notes" sort:deadline->title columns:State,Title
```

## Tokens

A query splits on spaces, tabs, newlines and `&` — four separators, runs
collapse. Inside `"double quotes"` separators are literal; the quotes
themselves never reach the value; an unclosed quote runs to the end. There
are no escapes: the only way to get a separator, a leading `-` or `+`, or a
colon into a value is quoting.

- A token starting with `"` is **free text whole**, key-shaped or not:
  `"state:TODO"` searches for that string.
- A token starting with `-` (before any other character) is **negated**.
  A lone `-` negates the match-everything empty term and so empties the table.
- A token starting with `+` (before any other character) is **added**: it
  widens its own key's axis instead of narrowing it (below). The sign is the
  first character alone, so a second one lands in the value: `+-x` adds the
  free-text needle `-x`, and `-+x` drops the rows carrying `+x`. A quoted
  `"+state:x"` is the literal string, `+` included. A lone `+` adds nothing.
- `key:value` splits on the first `:` or `=` (the two are aliases). The value
  may carry more colons: `title:a:b` looks for `a:b`.
- A key is one of the thirteen below, spelled exactly (keys are
  case-sensitive; `STATE:TODO` is free text). Anything else — `note:later`,
  a URL, `:work:` — is free text, matched literally, never an error.

## Free text

A bare word matches as a substring anywhere in the row's six view cells
(state, priority, title, scheduled, deadline, tags), case-folded, link
descriptions shown the way the table shows them. Several tokens AND, in any
order. A quoted token is one contiguous needle: `"the table"` matches where
`table the` (two tokens) may not — and cannot span from one cell into the
next.

The tags cell is org's own `:a:b:` spelling, sorted — so free text `web:`
finds every row tagged `web`, and `glance:web` matches where `web:glance`
does not.

## The six column keys

| key | matches | notes |
| --- | --- | --- |
| `state:` | the whole keyword | reads through org's brackets: `state:[#TODO]` ≡ `state:TODO`; `state:TOD` matches nothing |
| `priority:` | the letter | `priority:A` ≡ `priority:[#a]` |
| `title:` | substring of the title | |
| `scheduled:` | **prefix** of the date, or a comparison | `scheduled:2026-08` is a month; `scheduled:<2026-09` is before September |
| `deadline:` | **prefix** of the date, or a comparison | `deadline:2026-09-15..2026-10-07` is a range — see Comparisons |
| `tag:` | substring of the `:a:b:` cell | `tag:glan` matches `glance`; two `tag:` tokens intersect |

Values are case-folded. An empty value (`state:`) narrows nothing — a
half-typed token never empties the table.

## Two more predicates

- **`planned:`** — both date cells at once: a row is planned when either
  SCHEDULED or DEADLINE holds anything (`CLOSED:` does not count). Prefix
  dates like the date keys; `planned:*empty*` is the undated rows;
  `-planned:*empty*` the dated ones. Comparisons and ranges read here too,
  each date cell answering in turn — and the range says on this key what two
  tokens cannot (below).
- **`substring:V`** — free text under a key, exactly `V` as a bare token
  would match, but with the predicate machinery: `substring:"-x"` searches a
  leading hyphen, `-substring:x` negates, `substring:a|b` alternates.

## References: `ref:` and `from:`

A REFERENCE is a link in a row's subtree that resolves to another row. Two
keys read one edge, each from its own end.

- **`ref:ID`** — rows whose subtree links to the row with that
  `ORG_GLANCE_ID`. Links resolve against the target's id and title, so
  `[[Title]]` counts; an `[[id:…]]` link is org-id's and matches only the
  target's `:ID:` property. `@` on a focused row drills into `ref:ID` behind a
  breadcrumb; `DEL` pops back.
- **`from:ID`** — the REVERSE: the rows that row links to. Its own links
  resolve through those same two namespaces — every row's id and title for a
  `[[glance:…]]` or `[[Title]]` link, the `:ID:` properties for an `[[id:…]]`
  one — so a link naming no row (a `https:` bookmark, a `file:` attachment) is
  no reference and brings nothing back.

The id is **the one value that is not case-folded**, on both keys, and an
unknown id matches nothing. **A row is neither its own reference nor its own
from-target:** a link resolving back to the row it was written in is dropped at
both ends.

### One edge, two tokens

Two rows, one edge — `Ship the release` carries
`[[glance:def456?kind=blocked-by][Sign the contract]]` in its body:

| row | `ORG_GLANCE_ID` |
| --- | --- |
| Ship the release | `abc123` |
| Sign the contract | `def456` |

| query | serves | reads |
| --- | --- | --- |
| `ref:def456` | Ship the release | the rows pointing AT `def456` |
| `from:abc123` | Sign the contract | the rows `abc123` points at |

`ref:def456` names that edge from the target's end and `from:abc123` from the
source's. Neither serves the row it was asked about.

### `?kind=SLUG`, the edge's own spelling

An edge may carry a KIND, and the query spells it the way the FILE does:
org-glance writes `[[glance:ID?kind=SLUG][…]]`, so the token is
`ref:ID?kind=SLUG` and `from:ID?kind=SLUG`. One grammar, read the same on both
sides.

- The bare forms stay **kind-blind** — `ref:ID` is every edge into `ID`, plain
  mentions included — so a query written before kinds means what it meant.
- The `?` opens the kind exactly as it does in a link target — and the cut is
  taken **only where a `kind=` comes out of it**, so an id carrying a `?` that
  declares no kind stays whole and resolves to the row it always did. A title's
  own `?` is text for the same reason.
- The slug is the peer's: downcased, whitespace runs folded to one `-`, applied
  on the write and on the read, so `?kind=Blocked By` and `?kind=blocked-by`
  are one kind. The scanner cuts on the space, so the long spelling is quoted
  whole — `ref:"def456?kind=Blocked By"`.
- A kind no edge carries matches nothing, the way `state:TOD` does.

On the pair above, `ref:def456?kind=blocked-by` still serves Ship the release,
and `ref:def456?kind=see-also` serves nothing.

### `*any*`, the existence meta

`*any*` stands where an id stands, and asks whether the row is on the relation
at all:

| token | serves |
| --- | --- |
| `ref:*any*` | rows carrying at least one reference — something they point at |
| `from:*any*` | rows something points at |
| `-ref:*any*` | rows that reference nothing |
| `-from:*any*` | the ORPHANS: rows nothing points at |

**It is the union over the anchor**, which is what every starred word in a value
slot already means: `ref:*any*` serves exactly the rows some `ref:ID` serves,
and `from:*any*` the rows some `from:ID` serves. Two laws follow rather than
being added. **A self-reference is still no reference** — a row whose only link
points at itself is served by neither, exactly as it is served by neither when
asked with its own id. And **a link naming no row is still no reference**: an
unknown anchor serves nothing, so a `https:` bookmark counts towards neither.

It is a starred word, so it is one of the metas below and reserved in every
context — and since a reference value is the one value that is not case-folded,
the stars are spelled in lower case: `ref:*ANY*` names no row and matches none,
the way `ref:ALPHA` does. The bare word stays an ordinary value, so `ref:any` is
the id spelled `any`.

A kind narrows the meta like any other anchor. With `blocked-by` written on the
blocked row, `ref:*any*?kind=blocked-by` is the rows carrying such an edge —
everything that is blocked — and `from:*any*?kind=blocked-by` the rows some edge
names that way: everything that blocks.

### Two keys, two axes

`ref` and `from` are DIFFERENT RELATIONS, so each carries its own axis. Two
tokens on one key AND, a `+` widens the axis it names and no other, and the two
axes AND with each other as any two do. Grouping is by key and never by
adjacency, so token order carries nothing here either.

| query | serves |
| --- | --- |
| `ref:abc123 ref:def456` | rows pointing at both |
| `ref:abc123 +ref:def456` | rows pointing at either — one axis, widened |
| `ref:abc123 from:abc123` | rows that point at `abc123` and are pointed at by it |
| `ref:abc123 +from:def456` | those of them `def456` points at — a `+` alone on its axis is the plain token, and it never widens across to `ref` |

## Alternatives and negation

- `|` inside a predicate's value ORs alternatives, each read with the key's
  own rule: `state:TODO|DONE`. Empty alternatives drop; a value left with
  none narrows nothing. `|` in free text is literal.
- `-` negates the whole token, alternatives included: `-state:TODO|DONE`
  is "neither". Every narrowing token negates; `-sort:…` and `-columns:…`
  are refused.

## Adding: `+` widens its own axis

A token opening with `+` joins its key's axis as an alternative. Within one
axis the plain and negated tokens AND as they always did, and the `+` tokens
OR against that conjunction; the axes still AND with each other. So a `+`
widens one filter and leaves every other one standing.

An axis is a key: each of the six column keys has its own, so do `planned`,
`ref` and `from` — `ref` and `from` being different relations, they are two
axes and not one — and `substring:` shares one with bare free text. Grouping is
by key, never by adjacency, so token order carries nothing.

`priority:[#A] tag:book +priority:[#B]` serves the book rows at priority A or
B: the priority axis reads "A or B", the tag axis reads "book", and the two
AND.

Every narrowing key takes the sign, free text included — `milk +bread` serves
rows carrying either word. Alternatives and metas ride along:
`+state:DONE|CANCELLED` joins both, `state:TODO +state:*inactive*` widens with
a meta. A repeated `+` token changes nothing.

A `+` token alone on its axis is the plain token: `+state:DONE` ≡
`state:DONE` — a `+` widens against its own axis, never against the whole
query.

An empty value adds nothing and establishes no axis: `+state:` and `+state:|`
narrow nothing and widen nothing, so a half-typed token leaves the table as it
was. A lone `+` is the same non-event, where a lone `-` empties the table. A
half-typed PLAIN token on a widened axis establishes no axis either:
`state: +state:DONE` is the DONE rows, never every row.

| query | serves |
| --- | --- |
| `priority:[#A] +priority:[#B]` | rows at either priority |
| `tag:work +tag:home` | rows tagged work or home (two plain `tag:` tokens intersect) |
| `-state:TODO +state:DONE` | rows that are not TODO, plus the DONE ones |
| `planned:*empty* +planned:2026-08` | the undated rows plus the August ones |

The chip strip spells an added token as written — `+priority:[#B]`, and free
text as `+substring:bread`. Committing a token whose opposite-signed twin
already stands in the strip removes both: the strip's own affordance over a
pair the grammar answers as every row, `-state:DONE +state:DONE` being the
tautology ¬v ∨ v — a rule of the strip, never of the grammar.

In a URL query string a bare `+` decodes to a space, so the sign travels
percent-encoded as `%2B`: `?q=state%3ATODO%20%2Bstate%3ADONE`.

Order and shape never narrow, so they have nothing to widen: the three
shaping keys refuse the sign with HTTP 400, naming the offending token in its
`key:value` spelling.

| token | refused with |
| --- | --- |
| `+sort:…` | `a sort key cannot be added` |
| `+columns:…` | `a columns key cannot be added` |
| `+view:…` | `a view key cannot be added` |

## Comparisons on the date keys

`scheduled:`, `deadline:` and `planned:` take a COMPARISON in the value
position. The operator sits at the head of the value with no space, and the
longer spelling is read first, so `>=` is never `>` followed by an `=`.

| value | serves |
| --- | --- |
| `deadline:2026-09` | due in September — the prefix reading, unchanged |
| `deadline:<2026-09` | due before September |
| `deadline:<=2026-09` | due in September or earlier |
| `deadline:>2026-09` | due after September |
| `deadline:>=2026-09` | due in September or later |
| `deadline:2026-09-15..2026-10-07` | due between those two days, both ends included |

A date is any non-empty prefix of an ISO stamp, the same literal the bare form
takes: `2026`, `2026-08`, `2026-08-0` (a month's first nine days),
`2026-08-03 09`. Each names an INTERVAL, and the operator says which end it
cuts at — **`<` and `>=` cut at the interval's first instant, `<=` and `>` at
its last.** The bare form is the two inclusives at once:
`deadline:2026-09` ≡ `deadline:>=2026-09 deadline:<=2026-09`. No date
arithmetic happens anywhere — an interval's last instant is spelled as
everything its prefix reaches.

The operator is read on these three keys and nowhere else: `title:>x` is the
substring it always was, `tag:<a>` the tag search it always was. A value
carrying neither an operator nor `..` reads exactly as it read before. A
literal that does not open with a digit matches nothing, the way `state:TOD`
does, and an operator with no literal (`scheduled:>=`) narrows nothing, as any
half-typed token.

### `A..B`, the range

On a single-cell key the range is the two inclusives: `scheduled:A..B` ≡
`scheduled:>=A scheduled:<=B`, which two tokens on one axis already say (they
AND). **On `planned:` the range says what no pair of tokens can: ONE date cell
inside the interval.** `planned:2026-08-01..2026-08-31` serves the rows whose
SCHEDULED falls in August or whose DEADLINE does. `planned:>=2026-08-01
planned:<=2026-08-31` serves those and more — a row scheduled next year with a
deadline of last year passes both tokens, one cell answering each, and lies in
no August.

### `*today*` is a date

`*today*` stands wherever a date literal stands: bare, behind any operator, at
either end of a range. It is read ONCE per request, against the server's local
day, as `YYYY-MM-DD`.

| token | serves |
| --- | --- |
| `scheduled:*today*` | scheduled today — the prefix reading of today's date |
| `deadline:<*today*` | overdue |
| `planned:<=*today*` | planned by today, the overdue among them |
| `scheduled:*today*..*today*` | strictly today, said as a range |

The table in the browser answers the same words against the BROWSER's local
day. Server and page are one machine over loopback, so the two agree except
across a midnight the request itself straddles.

### A date can be shifted

A date value may carry a SHIFT — `BASE+N UNIT` or `BASE-N UNIT` — and the
primary spelling has no spaces in it, the token grammar owning those.

| token | serves |
| --- | --- |
| `scheduled:<=*today*+30d` | scheduled within the next thirty days |
| `deadline:>=*today*-7d` | due since a week ago |
| `planned:*today*..*today*+30d` | the thirty-day lookahead, in one token |
| `deadline:2026-09-15+2w` | the prefix reading of 2026-09-29 |

`BASE` is a date literal, `*today*`, or nothing at all, `N` a run of digits,
and `UNIT` one of org's own four: `d`, `w`, `m`, `y`.

**The shift resolves at COMPILE, to a plain day literal.** `*today*` becomes the
request's day and a spelled date becomes itself; `w` is seven days; `m` and `y`
are calendar arithmetic and **clip** — Jan 31 `+1m` is Feb's last day, never
March 3. The sum is written down once, before any row is asked.

After that, every law on this page applies to it untouched: the granularity
cuts, the empty cell outside every comparison, negation no mirror, the ranges,
the alternatives, the signs. **A shifted value is one more spelling of a day
literal, never a new kind of value.**

- **Both ends of a range take one**, and either may be plain:
  `scheduled:*today*-7d..*today*+7d` is the fortnight around today.
- **A bare shift is today-relative.** `scheduled:+30d` is
  `scheduled:*today*+30d`, the reading the planning grammar already gives a bare
  `+3d` ([commands.md](commands.md#dates)).
- **The token's sign is still the token's.** The first character alone is the
  sign, so in `+scheduled:+30d` the leading `+` widens the scheduled axis and
  the value's own `+` is the shift's.
- The sign that opens a shift is the one before the unit, so a date's own
  hyphens are never mistaken for it: `deadline:2026-09-15-7d` is the week
  before that day.
- **`*today*` with no clock behind it names no day**, shifted or bare, and so
  matches no row.
- **A half-typed shift narrows nothing.** `scheduled:*today*+` and
  `scheduled:*today*+30` are the half-typed family: unsigned they leave the
  table as it was, negated they empty it, as `-state:` does.

The quoted value form is the same token with room to breathe. A quoted value
after a key is a predicate — the quotes never reach the value and separators
inside them are literal — so `scheduled:"<= *today* + 30 days"` is
`scheduled:<=*today*+30d` said long: spaces beside the operator, the range mark,
the sign and the unit, and the unit spelled as a word (`day`, `week`, `month`,
`year`, singular or plural), case-folded like every value. A pre-pass folds the
quoted spelling onto the compact one, so there is ONE parser and one law. The
fold is no blanket space-strip: the one space a date literal owns, between its
day and its hour, stands — `scheduled:"2026-08-03 09"` is the hour it always
was.

The lookahead is what an agenda is usually asked for, so it is what a tree pins
into the agenda view ([config.md](config.md#a-today-agenda)):

```
#+GLANCE_AGENDA_FILTER: state:*active* planned:*today*..*today*+30d sort:scheduled
```

### Two guards

- **The empty cell sits outside every comparison.** An undated row passes no
  `<`, `<=`, `>` or `>=`: byte order would sort an empty cell before every
  date, which says nothing true about the row. `*empty*` stays the only name
  for those rows, and `+` puts them back:
  `deadline:<2026-09 +deadline:*empty*`.
- **Negation is no mirror.** `-scheduled:<*today*` and `scheduled:>=*today*`
  differ on exactly the undated rows: the negation carries them, the comparison
  leaves them out. The four operators do not pair off under the sign.

`|` splits before the operator is read, so `scheduled:<2026-08|>2026-09` is two
comparisons ORed and never a range; `-` inverts the whole token, as everywhere.

## The `*word*` metas

Seven, spelled with matched stars; a star anywhere else is literal text and
never a glob.

| meta | where | means |
| --- | --- | --- |
| `*empty*` | any column key, `planned` | the named cells are empty |
| `*active*` | `state:` | an active keyword **or no keyword** — stateless rows are live work |
| `*inactive*` | `state:` | a done-like keyword (the empty cell is not included) |
| `*archive*` | `tag:` | the whole tag `archive` — see below |
| `*today*` | the date keys' values | the server's local day, `YYYY-MM-DD` |
| `*any*` | `ref:`, `from:` | any target at all: the row is on that relation |
| `*none*` | `sort:` | no order at all: document order |

`*today*` and `*any*` stand inside a value where the rest stand as the whole of
one — a date literal's place and an id's place — and they are members all the
same: a starred word is a meta wherever it is read, or it is nothing.

The bare words stay ordinary values: `state:none` looks for a keyword
spelled NONE.

## `sort:`

```
sort:COLUMN[:desc][->COLUMN[:desc]]…
```

- Chain keys are the six column keys only. `->` is sugar for writing several
  `sort:` tokens; written order is the chain's order, and a repeated column
  keeps its **first** spelling, direction included.
- Directions: nothing or `:asc`, and `:desc`. Empty cells always sort last,
  whatever the direction; ties keep document order.
- The default chain — state → title → deadline → scheduled, state in the
  tree's own `#+TODO:` order — stands until any `sort:` token appears, and
  is then replaced whole.
- `sort:*none*` is the empty chain: document order. It stands alone — any
  companion that orders something is refused.
- A half-typed `sort:` or trailing `->` is the empty chain too (document
  order), never an error.

Refused with HTTP 400, naming the offending token in its `key:value` spelling:
a negated sort, `|` in a segment, an unknown column, a direction other than
asc/desc, `*none*` with a direction or a companion. A refusal is the whole
query's — a good token does not rescue a bad one.

## `columns:`

```
columns:Name,Name,…
```

- Comma-separated, written order, first spelling wins on a repeat
  (case-insensitively). Names resolve against each builtin's key **and**
  header — `State`, `#`, `Title`, `Scheduled`, `Deadline`, `Tags` — so the
  header you see is a name you can write.
- Any other name is a **custom column**: its cells read the row's property
  drawer by that key (`columns:owner`), except `closed`, which reads the
  planning line's `CLOSED:` stamp. Custom cells are read-only and are not
  sortable chain keys.
- `Title` is always present: unnamed, it is put in front; named, it stays
  where you put it.
- An empty list (`columns:`) falls back to the default six. Negation and
  `|` are refused.

## `view:NAME`

A saved view's name stands for its whole query. The token expands in the
shell before the fetch — the view's own string replaces the query — so a
name nobody carries simply stays text. Names are `[A-Za-z0-9_-]`, the token
must stand alone, and the first `view:` in the string is the one read.

Three names exist, each a pragma in the tree's config layer
(`.org-glance/config/system.org`), each with a built-in fallback:

| name | pragma | built-in |
| --- | --- | --- |
| `default` | `#+GLANCE_DEFAULT_FILTER` | `state:*active*` |
| `agenda` | `#+GLANCE_AGENDA_FILTER` | `state:*active* -planned:*empty* sort:scheduled` |
| `archive` | `#+GLANCE_ARCHIVE_FILTER` | `tag:*archive*` |

The agenda's built-in serves every dated row; rewriting it for the day's work
is [config.md](config.md#saved-views).

`g` applies the default (the view the tree opens on — also applied at boot
when the address bar carries no query), `a` the agenda, `P` pins the current
query into a saved view. The last pragma line in a file wins; the first
config layer that names one wins across directories.

## The archive

A tree that carries the tag `archive` anywhere hides archived rows from
every query that does not name them: `tag:*archive*` (or its negation)
disarms the hiding; the literal `tag:archive` does not. The withheld count
rides the `X-Glance-Archived` response header.

## Order, duplicates, precedence

- Narrowing tokens AND in any order; only `sort:` and `columns:` read their
  written order.
- A repeated predicate key ANDs: `state:TODO state:DONE` is nothing (one
  cell cannot be both), `tag:web tag:glance` intersects (a row carries many
  tags). A `+` on that key widens its axis instead, as above.
- `sort:`/`columns:`/`view:` never narrow: each serves the full set and
  leaves narrowing to the tokens beside it.

## Errors

Only the shaping keys refuse, naming the offending token in its `key:value`
spelling: `sort:` and `columns:` as above, negations (`-sort:…`, `-columns:…`)
among them, and the three `+` refusals as the refusal table under Adding spells
them. Everything else that fails to parse is free text; everything half-typed
narrows nothing. `?order=` is gone from the wire — using it is a 400 naming
`?q=sort:…` as the replacement.
