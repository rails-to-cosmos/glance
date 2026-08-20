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
are no escapes: the only way to get a separator, a leading `-` or a colon
into a value is quoting.

- A token starting with `"` is **free text whole**, key-shaped or not:
  `"state:TODO"` searches for that string.
- A token starting with `-` (before any other character) is **negated**.
  A lone `-` negates the match-everything empty term and so empties the table.
- `key:value` splits on the first `:` or `=` (the two are aliases). The value
  may carry more colons: `title:a:b` looks for `a:b`.
- A key is one of the twelve below, spelled exactly (keys are
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
| `scheduled:` | **prefix** of the date | `scheduled:2026-08` is a month |
| `deadline:` | **prefix** of the date | |
| `tag:` | substring of the `:a:b:` cell | `tag:glan` matches `glance`; two `tag:` tokens intersect |

Values are case-folded. An empty value (`state:`) narrows nothing — a
half-typed token never empties the table.

## Three more predicates

- **`planned:`** — both date cells at once: a row is planned when either
  SCHEDULED or DEADLINE holds anything (`CLOSED:` does not count). Prefix
  dates like the date keys; `planned:*empty*` is the undated rows;
  `-planned:*empty*` the dated ones.
- **`ref:ID`** — rows whose subtree links to the row with that
  `ORG_GLANCE_ID` (a row never references itself, and links resolve against
  the target's id and title, so `[[Title]]` counts). An `[[id:…]]` link is
  org-id's and matches only the target's `:ID:` property. The one value that
  is **not** case-folded. An unknown id matches nothing. `@` on a focused row
  drills into `ref:ID` behind a breadcrumb; `DEL` pops back.
- **`substring:V`** — free text under a key, exactly `V` as a bare token
  would match, but with the predicate machinery: `substring:"-x"` searches a
  leading hyphen, `-substring:x` negates, `substring:a|b` alternates.

## Alternatives and negation

- `|` inside a predicate's value ORs alternatives, each read with the key's
  own rule: `state:TODO|DONE`. Empty alternatives drop; a value left with
  none narrows nothing. `|` in free text is literal.
- `-` negates the whole token, alternatives included: `-state:TODO|DONE`
  is "neither". Every narrowing token negates; `-sort:…` and `-columns:…`
  are refused.

## The `*word*` metas

Five, spelled with matched stars; a star anywhere else is literal text and
never a glob.

| meta | where | means |
| --- | --- | --- |
| `*empty*` | any column key, `planned` | the named cells are empty |
| `*active*` | `state:` | an active keyword **or no keyword** — stateless rows are live work |
| `*inactive*` | `state:` | a done-like keyword (the empty cell is not included) |
| `*archive*` | `tag:` | the whole tag `archive` — see below |
| `*none*` | `sort:` | no order at all: document order |

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

Refused with HTTP 400, naming the token as written: a negated sort, `|` in a
segment, an unknown column, a direction other than asc/desc, `*none*` with a
direction or a companion. A refusal is the whole query's — a good token does
not rescue a bad one.

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

`g` applies the default (the view the tree opens on — also applied at boot
when the address bar carries no query), `A` the agenda, `P` pins the current
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
  tags).
- `sort:`/`columns:`/`view:` never narrow: each serves the full set and
  leaves narrowing to the tokens beside it.

## Errors

Only `sort:` and `columns:` refuse, as above, with the offending token named
as written. Everything else that fails to parse is free text; everything
half-typed narrows nothing. `?order=` is gone from the wire — using it is a
400 naming `?q=sort:…` as the replacement.
