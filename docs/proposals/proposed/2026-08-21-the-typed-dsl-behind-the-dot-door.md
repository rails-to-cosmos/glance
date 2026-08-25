# Proposal — the typed DSL behind the `.` door

**Status:** proposed · **Date:** 2026-08-21 · **Origin:** user, picking variant
F out of the dot-chain spike — *"the `.` door's box becomes the typed DSL the
spike proved — variant F on variant D's pill mechanics. Haskell-flavored:
`.filter(state = Active, tag /= "chore") .sort(deadline) .columns(State, Title)`
as stage pills; record kwargs; capitalized constructors for the closed meta
roster — closed at the metas, OPEN at the keywords, `state = "TODO"` a string
because keywords are the tree's own; `-` and `+` are sign KEYS that expand to
structure; opened quoted slot on the equals; comma separators; dry final RET;
`/` edits the standing filter stage; DEL erases the latest stage whole. The flat
`?q=` stays the ONE truth — the DSL is a view; the lisp normal form is the
mechanized agreement proof."*

**Reviewed in three passes, 2026-08-21.** The mandate above is quoted as it was
given, superseded signatures included; the body carries the amended ones. The
passes settled: the signatures and the column-name spelling
([§0](#0--the-roster-law-which-decides-every-spelling), [L2](#l2--grammar));
datetime comparisons, gated on
[the flat half](2026-08-21-datetime-comparisons-in-the-flat-grammar.md)
([L8](#l8--datetime-comparisons)); the denotation as a function algebra
([L5](#the-denotation-is-a-function-algebra)) and the context as data
([L9](#l9--the-context-is-data)).

The spike is [`docs/spikes/2026-08-21-dot-chain-box/`](../../spikes/2026-08-21-dot-chain-box/README.md);
six tabs, argued in sixteen rounds, every amendment pinned in its `check.mjs`.
This proposal synthesizes that argument into a landing. It re-derives nothing.

## The composition law

The `.` door composes a chain of typed stages, and the flat `?q=` string is
what it composes and the only thing it composes.

```
.filter(state = Active, tag /= "chore")  .sort(columns = ["Deadline"])  .columns("State", "Title")

  ⇓ the composer, before the wire

state:*active* -tag:chore sort:deadline columns:State,Title
```

The chain is a VIEW of the string, the way `/` is a view of the string's filter
half ([slash-filters-dot-expression](../done/2026-08-20-slash-filters-dot-expression.md):22).
The server never learns a stage: `Filter.hs`, `Sort.hs` and `Columns.hs` read
the same bytes they read today, and the wire is untouched.

## The surface

```haskell
.filter(state = Active, tag /= "chore", priority = ["A", "B"])
.sort(columns = ["Deadline", Desc "Title"])
.columns("State", "Deadline")
```

Each stage takes **args and kwargs**: a kwarg BINDS a field the stage can carry,
a positional arg is the thing the stage is ABOUT. The three signatures and why
they differ are [L2](#l2--grammar)'s.

### §0 — the roster law, which decides every spelling

**A bare word names something the LANGUAGE closes; a double-quoted string names
something a TREE opens.** That one rule decides the whole surface, and it is why
the column names sit on the string side. Since the
language folds case ([L1](#l1--lexical-structure)), the QUOTES carry the whole
distinction — the capitalization is display, not meaning.

| roster | closed by | spelling |
| --- | --- | --- |
| the starred metas — `Active`, `Inactive`, `Empty`, `Archive`, `None`, `Today` | the language: `AGENTS.hs`:2501-2521, "the starred family, and it is total" | bare word, displayed capitalized |
| the two directions — `Asc`, `Desc` | the language: "nothing or `:asc`, and `:desc`" (`docs/query.md`:346-347) | bare word, displayed capitalized |
| the nine field names | the language: `docs/query.md`:32-34, the narrowing keys | bare word, displayed lowercase |
| TODO keywords | the TREE, out of its own `#+TODO:` line | string |
| tags, titles, refs, free text | the tree | string |
| **column names** | **nobody** — see below | **string** |

The whole predefined vocabulary is enumerated in
[L3, the prelude](#l3--the-prelude); the per-field domains are
[L4](#the-field-roster-and-its-domains). This subsection is the ARGUMENT; those
two are the definition, and each fact lives in one of them.

**Columns are open.** "Any other name is a **custom column**: its cells read the
row's property drawer by that key (`columns:owner`)" (`docs/query.md`:371-374),
and `Q.resolveColumns ["state", "tag", "Effort"]` resolves a name no builtin
carries (`test/TestSpec.hs`:979-988). A constructor roster for columns would
have to close a set the grammar deliberately leaves open, so column names sit on
the string side with the keywords: `.columns("State", "Effort")`.

**And a column is a string EVERYWHERE.** The flat `sort:` restricts its chain to
the six column keys (`docs/query.md`:343; `Sort.hs`:60-61 refuses anything else
by name), so the sortable set really is closed — and spelling it differently
from `.columns(…)`'s open set would make `Deadline` and `"Deadline"` two
different things that are the same thing. One vocabulary, one spelling. Sort's
restriction is a LEGALITY CHECK on the value, the way `state = Archive` is a
type error (the [refusals table](#the-refusals-by-tier)), never a change of
spelling.

### The rest of the surface

- **Record kwargs.** `state = Active`, spaces around the `=`. A field is a key
  the grammar already has — the nine narrowing keys of `docs/query.md`:32-34 —
  so the surface can name nothing the flat string cannot. `=` is already the flat grammar's own
  key separator, aliased to `:` (`docs/query.md`:30), so the field spelling is
  the flat spelling with the spaces put in.
- **Constructors are ONE shared sum, never one per field.** `*empty*` is legal
  on all six column keys and on `planned` (`docs/query.md`:327), so per-field
  types would need qualified names for a distinction the grammar does not make.
  The FIELD decides which constructors are legal, which is the Haskell reading.
- **Double-quoted literals for the open values.** `state = "TODO"`, because the
  keywords are the TREE's. A quoted string is a literal and never a sign:
  `tag = "-chore"` searches a tag spelled `-chore`, which is `substring:"-x"`'s
  rule said in Haskell (`docs/query.md`:72).
- **Lists for the alternatives.** `state = ["TODO", "DONE"]` composes to
  `state:TODO|DONE`. The bare list is `Any`; `All` is the intersection (§5).
- **`/=` for the negation**, the Prelude's own, scoping the WHOLE token,
  alternatives included — the flat grammar's De Morgan pin (`docs/query.md`:186).
  `not (…)` is accepted as the wrapper an operator cannot carry.
- **Free text is `substring = "milk"`,** with a bare `"milk"` the same thing.
  Both compose to `substring:milk`; the axis the additive proposal calls `text`
  is `substring:`'s own ([additive-filters](../done/2026-08-20-additive-filters.md):98).
- **`raw "…"` is the escape hatch**, and the surface admitting it is not total
  (§4).

### `.sort(…)`, and the direction spelling

**The direction is `Desc "Title"` — the closed constructor applied to the open
string.** `.sort(columns = ["Deadline", Desc "Title"])` composes to
`sort:deadline->title:desc`. Three reasons, and each is the flat grammar's:

1. **The direction is PER SEGMENT.** `sort:state:desc->title` and
   `sort:state->title:desc` are different orders (`TestFilter.hs`:413-414), so
   the direction has to ride the ELEMENT. A stage-level `order = Desc` kwarg, or
   a second list paired by index, would both say less than the string says.
2. **It is §0 twice over, with nothing bent.** `Desc` is closed by
   `docs/query.md`:346-347; `"Title"` is open. The constructor is the language's
   and its argument is the tree's, which is the same figure as
   `state = Active` beside `state = "TODO"`.
3. **`Asc` is spellable and never required,** matching "nothing or `:asc`". A
   bare `"Deadline"` is ascending, and the composer emits no `:asc`, so the
   round-trip prints what the reader typed.

Rejected: `["Deadline:desc"]` — the flat suffix smuggled into a literal, which
makes a string into a mini-grammar and puts a `:` inside a value the surface
otherwise treats as opaque.

**The empty chain is `columns = None`,** and `columns = []` normalizes to it —
document order can only be SPELLED, never left absent ([L2](#l2--grammar)).

**The stage normalizes a name to its key.** `Sort.hs`:60-61 refuses any segment
that is not one of the six lowercase keys, so `.sort(columns = ["Deadline"])`
composes `sort:deadline`. The surface accepts the header the reader sees
(`docs/query.md`:368-370) and emits the key; rendering back prints the header,
so the round-trip is stable. **The surface never composes a `sort:` the flat
reader would refuse** — normalizing a spelling is not the same as composing a
stage the string cannot carry, and the second is forbidden by §10's invariant.

### The keys

| key | inside the parens | on the strip |
| --- | --- | --- |
| `.` | types (a dot is legal in `title:v1.2`) | chains the next call |
| `(` | — | takes the offered call, and opens the stage's first slot |
| `,` | separates arguments, and opens the next slot | — |
| `)` | closes the stage onto the strip | — |
| `TAB` | takes the offer (finality: the dry law's edge, below) | — |
| `-` | flips the kwarg under the caret `=`↔`/=`; on empty ground spawns `not (\|)` | — |
| `+` | opens the value into a list with a fresh slot: `state = ["TODO", \|]` — the alternation, which on a bare axis is what the flat `+` means (law 5's agreeing half) | — |
| `/` | inside a quoted string, a character (`title = "a/b"`) | reopens the standing `.filter(…)` onto a FRESH argument — the comma appended, that position's offers standing; an abandoned slot leaves no trace at the close (spike rounds 12–13). The rewrite is in place ([§2](#2-the-door-swap)) |
| `DEL` | the box's own backspace | erases the latest stage whole ([§6](#6-del)) |
| `RET` | — | applies the composed string |
| `ESC` | cancels the input whole — the edit, never the menu | — |

`-` and `+` are sign KEYS that expand to structure; the sign is never a
character here ([L1](#l1--lexical-structure)).

**Every slot opens quoted, positional slots included.** The spike's round 7 opens
`state = "|"` on the equals so the reader types the value and never the
punctuation (spike README:479-486); the amendment generalizes it. In a stage
whose argument position takes a STRING — `.columns(`'s every slot, the elements
of `.sort(…)`'s list, `.filter(…)`'s free-text position — `(` and `,` land the
caret inside a fresh `"|"` the same way. The offers over such a slot lead with
the constructors where any are legal (`Desc` inside the sort list, nothing
inside `.columns(…)`), and **accepting a constructor swallows the quotes** where
accepting a literal keeps them: a constructor is no string. **The dry law's edge
is the value, never the position** (spike round 11): an accept that finishes a
term is final and closes the offers, and one that leaves the caret inside what
it wrote — a key's fresh slot, `not (|)`, a list's next element — re-offers at
once, since the reader was moved somewhere new and a position's offers stand.
So `.columns(` yields `.columns("|")` with the column names offered over it,
and `Deadline` taken over the sort list's slot yields `["Deadline"]` closed and
final, while `Desc` taken there yields `[Desc "|"]` with the caret in the
constructor's own slot and that slot's offers open.

## The language, defined

The DSL is designed as a proper language, and this is the proposal's spine; the
decisions that follow (§1–§10) are what shipping it costs. The register is
[additive-filters](../done/2026-08-20-additive-filters.md)'s formal-semantics
section, carried through to a whole language.

**Section map:** L1 lexical structure · L2 grammar · L3 the prelude · L4 static
semantics · L5 dynamic semantics · L6 diagnostics · L7 evolution · L8 datetime
comparisons · L9 the context.

L0 governs all nine, and every section is a consequence of it:

> **L0 — the governing law. The surface can spell nothing the flat string
> cannot,** and everything it spells, the flat string carries.

### L1 — Lexical structure

#### Case: the language folds it

**The DSL is case-insensitive.** Stage names, field names and the prelude's own
words all fold: `state`, `State` and `STATE` are one name, and `active`,
`Active` and `ACTIVE` are one constructor. This is a departure from the flat
grammar, where "keys are case-sensitive; `STATE:TODO` is free text"
(`docs/query.md`:32-34), and the departure never leaks: **the composer emits the
canonical lowercase key**, so the flat reader is handed the only spelling it
accepts. A reader who WANTS the free-text reading writes it as a positional
string — `.filter("STATE:TODO")` composes `substring:STATE:TODO`, exactly what
the flat reader does with the bare token.

The consequence for the lexer is the interesting one. With case folded there is
no lexical split between identifiers and constructors, so **there is one word
token and position resolves it.**

#### The tokens

| token | shape | notes |
| --- | --- | --- |
| `word` | `letter { letter \| digit \| "_" }`, folded | ONE class. Position plus the closed world decide whether it is a stage, a field, a kwarg name or a prelude constructor |
| `string` | `'"' { char - '"' } '"'` | the open half of every roster. No escapes — see below |
| `punct` | `.` `(` `)` `[` `]` `,` `=` `/=` `<` `<=` `>` `>=` | `/=`, `<=` and `>=` each lex as ONE token, longest-first; a `/` not followed by `=` is a lex error outside a literal, where a bare `<` or `>` is the strict operator ([L8](#l8--datetime-comparisons)) |
| — | whitespace | insignificant outside literals |

**There are no number literals.** A date is a string (`deadline = "2026-08"`)
because the flat grammar matches a date by PREFIX on the cell's text
(`docs/query.md`:56-57), never numerically; a priority is a string for the same
reason. Admitting a numeric literal would admit a value with no flat image. **A
date under a comparison is a string too** — `deadline < "2026-09"` — for exactly
the same reason: the flat comparison compares the cell's ISO TEXT, and its
literal is a date PREFIX rather than a number ([L8](#l8--datetime-comparisons)).

**There are no comments,** and no line structure: a chain is one expression.

**There are no escapes in a string literal,** and the reason is the governing
law. The flat grammar has none — "the only way to get a separator, a leading `-`
or `+`, or a colon into a value is quoting" (`docs/query.md`:18-19) — and the
quotes themselves never reach the value, so **no flat VALUE can contain a
quote.** An escape here would spell a value the string cannot carry. A `"` inside
a value is unspellable in both languages, which is one language's property said
twice.

**`raw` is the one exception, and it is exact.** Its argument is flat query
TEXT rather than a value, and query text legally contains quote characters
(`substring:"-x"`). So `raw`'s literal — and only `raw`'s — admits the doubled
quote `""` for one `"`. The escape is admissible precisely because its content
is re-lexed by the flat reader under the flat quoting law, so nothing new is
spellable. Rejected reading: `raw` taking the remainder of the stage verbatim,
which is total too but forces `raw` to be the last argument for a rule that only
has to cover a quote.

**An unclosed literal runs to the stage's closing paren,** the flat grammar's
own "an unclosed quote runs to the end" (`docs/query.md`:16-17). A half-typed
value is never a lex error, because a half-typed token narrows nothing rather
than failing (`docs/query.md`:60-61).

**`-` and `+` are not tokens.** They are sign KEYS, handled by the editor before
the lexer sees anything (§the keys); inside a literal they are ordinary
characters. The surface has no sign, which is the whole point of a typed one.

**`.` is unambiguous here, and that is a gain the typed surface buys.** The
spike's plain-text variants had to answer "a dot inside the parens has to TYPE"
(spike README:742-745); in F every argument position is a word, a punct or a
delimited literal, so a `.` can only occur inside a literal. The chain operator
and the character are lexically apart.

#### Reserved

Reservation is STRUCTURAL, and it is a rule rather than a list. A bare word
resolves only against the closed world (L3's prelude plus the nine field names); a tree's own
vocabulary — keywords, tags, titles, ids — lives in the quoted position and can
never reach a bare one. **So nothing a tree contains can shadow a prelude name,
and no reserved-word list is needed.**

The cost is stated rather than hidden: a tree whose `#+TODO:` line defines a
keyword literally spelled `ACTIVE` reaches it as `state = "ACTIVE"`, and the
bare `active` is the meta forever. The bare position belongs to the language.

`columns` is both a stage name and `.sort(…)`'s kwarg name; the positions are
disjoint, so the two never compete. `sort`, `columns` and `view` are flat KEYS
that are NOT DSL fields — the first two are stages and the third is a shell
pragma (§L4).

#### Canonical display

**Any case is accepted, canonicalized on the dry accept, and displayed in the
Haskell convention — lowercase for the stage functions and the field names,
Capitalized for the constructors.** `.filter(state = Active)`, whatever was
typed. This is the one call the user may flip without touching a law; the
ALL-CAPS alternative and the argument that decides it are in **Alternatives
considered**.

Three grounds:

1. **Org's own vocabulary is upcased.** A tree may define `ACTIVE` as a TODO
   keyword. `Active` beside `"ACTIVE"` keeps the constructor and the keyword
   apart at a glance; `ACTIVE` beside `"ACTIVE"` puts the whole distinction on
   two quote marks, which is exactly the pair a reader skims past.
2. **It is the shape the idiom already carries.** Haskell capitalizes
   constructors and lowercases functions, and the surface is Haskell-flavored by
   mandate.
3. **The accept is already a formatter.** The dry accept rewrites what it
   inserts (spike round 3), and the opened slot already swallows quotes for a
   constructor and keeps them for a literal (round 7). Canonicalizing case is
   the same gesture, one keystroke later.

### L2 — Grammar

EBNF. The grammar is deliberately permissive wherever a type error reads better
than a parse error, and the permission is uniform: **a bare `word` parses in
every position a name may stand in, and every closed roster is resolved by
[L4](#resolution-by-position) rather than enumerated here.** So an unresolvable
word is `no value is called 'x' — did you mean "x"?` instead of a red paren, a
positional after a kwarg is named by the order rule, a second `.sort(…)` kwarg
is named by the signature rule, and `cmp-op` parses on any field so the checker
can say `'<' is no operator for 'title'`
([L4](#the-judgments), [L8](#l8--datetime-comparisons)).

```ebnf
(* a query is a chain of stages *)
query         = { stage } ;

stage         = filter-stage | sort-stage | columns-stage
              | defined-stage ;              (* v2 only — L9 *)

(* the three stage signatures *)
filter-stage  = "." "filter"  "(" [ filter-args  ] ")" ;
sort-stage    = "." "sort"    "(" [ sort-args    ] ")" ;
columns-stage = "." "columns" "(" [ columns-args ] ")" ;

(* a user definition, referenced by name.  L4 refuses a non-empty argument
   list until the deferred parameterized form lands — L9 *)
defined-stage = "." definition-name "(" [ positional { "," positional } ] ")" ;
definition-name = word ;                     (* resolved in the environment *)

(* .filter(*args, **kwargs) — order-blind; L4 imposes positionals-first *)
filter-args   = filter-arg { "," filter-arg } ;
filter-arg    = positional | kwarg ;
positional    = string | word raw-string ;    (* word: raw *)
kwarg         = binding | word "(" binding ")" ;      (* word: not *)
binding       = field op value ;
op            = "=" | "/=" | cmp-op ;
cmp-op        = "<" | "<=" | ">" | ">=" ;   (* temporal fields only — L4, L8 *)

(* .sort(columns = chain) — L4 refuses a second kwarg by name *)
sort-args     = kwarg-name "=" chain { "," kwarg-name "=" chain } ;
kwarg-name    = word ;                        (* `columns' and no other, L4 *)
chain         = word | "[" [ segment { "," segment } ] "]" ;   (* word: None *)
segment       = word | string | word string ; (* word string: Asc/Desc *)

(* .columns(*names) — positionals only, no kwargs *)
columns-args  = name { "," name } ;
name          = word | string ;               (* a bare word is L4's diagnostic *)

(* filter values *)
value         = word | string | list | word list ;   (* word list: All/Any *)
list          = "[" [ value { "," value } ] "]" ;

field         = word ;                        (* one of the nine, L4 *)
word          = letter { letter | digit | "_" } ;     (* case-folded *)
string        = '"' { char - '"' } '"' ;
raw-string    = '"' { ( char - '"' ) | '""' } '"' ;   (* the one escape, L1 *)
```

The three stage names are terminals and every one of them folds, `filter` and
`FILTER` alike; `defined-stage` catches every other name, so `.foo(…)` parses
and [L4](#resolution-by-position) answers it. The closed rosters a `word` may
resolve to — `Active`, `Inactive`, `Empty`, `Archive`; `All`/`Any` applied to a
list; `Asc`/`Desc` applied to a string; `not` wrapping a binding; `raw` applied
to a raw string; `None` alone in a chain; `columns` as `.sort(…)`'s one kwarg
name — are [L3](#l3--the-prelude)'s and are resolved by
[L4](#resolution-by-position). They are written Capitalized in this document
because that is the canonical display.

**`defined-stage` is gated to v2** ([L9](#l9--the-context-is-data)); until then
the call position holds three names and `.mine()` is `no stage is called
'mine'`.

**The three signatures read as a symmetry.** `.filter(…)` takes both, because it
has both a set of predicates to bind and free text to carry. `.sort(…)` takes
kwargs alone, because a chain is one value that can be replaced whole and the
stage has room to grow a second field. `.columns(…)` takes positionals alone,
because a column set has nothing to say but its members.

**The empty parens are three different facts,** and the flat grammar is why:

| stage | composes | means |
| --- | --- | --- |
| `.filter()` | nothing | no narrowing added |
| `.sort()` | nothing | the DEFAULT chain stands — `sort:` is NOT emitted |
| `.columns()` | nothing | the default six, which `columns:` also gives (`docs/query.md`:377) |

The middle row is the asymmetry: the flat `sort:` IS the empty chain, document
order (`docs/query.md`:353-354), where an absent `sort:` is the default chain.
So document order can only be SPELLED, as `columns = None`.

### L3 — The prelude

Every predefined name the language ships with, whole. These names are the
language's own; L1's structural reservation is what keeps them so, and L7 is
where the roster may grow.

**This table is the language's half of the evaluation context**
([L9](#l9--the-context-is-data)): the closed layer, hardcoded because it IS the
grammar (§8). The tree's half already rides the wire, and the user's half is
L9's v2. The table is also the SPEC for the in-app prelude pane — one list, two
renderings (`docs/invariants.md`:148-153), so a name cannot exist in the parser
and be missing from the pane.

| name | kind | type | positions | flat image | evidence |
| --- | --- | --- | --- | --- | --- |
| `Active` | constructor, nullary | `Meta` | filter value, on `state` | `*active*` | `AGENTS.hs`:2503, :2510, `metaHome`:2529; `docs/query.md`:328 |
| `Inactive` | constructor, nullary | `Meta` | filter value, on `state` | `*inactive*` | `AGENTS.hs`:2503, :2511, `metaHome`:2530; `docs/query.md`:329 |
| `Empty` | constructor, nullary | `Meta` | filter value, on the six column keys and `planned` | `*empty*` | `AGENTS.hs`:2503, :2512, `metaHome`:2527 (`EveryCell`); `docs/query.md`:327 |
| `Archive` | constructor, nullary | `Meta` | filter value, on `tag` | `archive` (the plain word since 2026-08-25) | `AGENTS.hs`:2503, :2513, `metaHome`:2528 (`TagCell`); `docs/query.md`:330 |
| `None` | constructor, nullary | `Chain` | `.sort(columns = ·)` alone | `sort:*none*` | `AGENTS.hs`:2503, :2514, `metaHome`:2531 (`OrderToken`); `docs/query.md`:332; `Sort.hs`:19, :57-59 |
| `Today` | constructor, nullary | `Date` | a temporal field's value, bare or behind an operator ([L8](#l8--datetime-comparisons)) | `*today*` | `AGENTS.hs`:2503, :2515, `metaHome`:2534 (`DateValue`, the fifth home); `docs/query.md`:331 |
| `Asc` | constructor | `Str → Seg` | a `.sort(…)` chain element | the bare key, no suffix | `Sort.hs`:15-16 (`directions`); `docs/query.md`:346-347 |
| `Desc` | constructor | `Str → Seg` | a `.sort(…)` chain element | `KEY:desc` | `Sort.hs`:15-16; `docs/query.md`:346-347 |
| `Any` | constructor | `[τ] → Value` | filter value | `v₁\|v₂` | `docs/query.md`:183-185 |
| `All` | constructor | `[τ] → Value` | filter value | repeated tokens, `k:v₁ k:v₂` | `docs/query.md`:420-422; spike README:716-722 |
| `not` | function | `Binding → Binding` | filter kwarg, wrapping | flips the token's sign | `docs/query.md`:186-188 |
| `raw` | function | `RawStr → Positional` | filter positional | the string verbatim | §4; spike README:178-179, :707-715 |
| `filter` | stage function | `(*Str, **Binding) → Stage` | the call position | narrowing tokens, space-joined | `docs/query.md`:13-35 |
| `sort` | stage function | `(columns : Chain) → Stage` | the call position | `sort:…->…` | `docs/query.md`:337-359 |
| `columns` | stage function | `(*Str) → Stage` | the call position; also `.sort(…)`'s kwarg NAME | `columns:…` | `docs/query.md`:361-378 |

Fifteen names. The **nine field names** are predefined too; their roster and
their domains are L4's table, which is the one place they are spelled — a fact
several readers agree on lives in ONE list (`docs/invariants.md`:148-153).
`docs/query.md`'s twelve keys are nine narrowing plus three shaping; the surface
takes two of the shaping ones as stages and `view` as a pragma, so the DSL has
nine fields. The roster becomes ten when `closed` lands as a flat key, and L4's
table is where that shows ([L8](#l8--datetime-comparisons)).

The prelude gains no name from L8's OPERATORS. A comparison is an operator, and
operators live in L2's grammar rather than in this table — there is no `Before`
constructor, no `Range` function, nothing to complete over. It gains exactly one
name from L8's VALUES: `Today`, the sixth member of the starred family, whose
home is the fifth `MetaHome` — `DateValue`, the one that names no cell and reads
no row. Its position is the open question L8 rule 2 carries.

A bare word resolves against this table and the nine fields and against nothing
else, so no tree can shadow a prelude name ([L1, reserved](#reserved)); the case
fold does not weaken it, since the QUOTES carry the distinction.

**Every prelude row owes a conformance pair** (L7): a DSL spelling beside its
flat image, agreeing at the IR. Rows lacking one in the spike's corpus —
`Any` spelled out, `Asc`, and the three stage functions applied empty — are the
additions §10 names.

### L4 — Static semantics

#### The types

```
τ  ::=  Meta          -- a closed constructor of the starred family
     |  Str           -- an open literal
     |  Date          -- a Str the flat grammar reads as a date PREFIX (L8)
     |  [τ]           -- a list
     |  Value         -- what a field binds: Meta | Str | [Str]
     |  Binding       -- field op value
     |  Seg           -- one element of the order
     |  Chain         -- the whole order
     |  Positional    -- free text, or raw
     |  Stage | Query
```

`All` and `Any` construct a `Value` from `[Value]`, and `Asc`/`Desc` a `Seg`
from a `Str` ([L3](#l3--the-prelude)); they are constructors, never types.

#### Resolution, by position

A bare word is resolved by where it stands, and by nothing else:

| position | resolves against | otherwise |
| --- | --- | --- |
| after `.` | `filter` \| `sort` \| `columns` | `no stage is called 'x'` |
| left of `=` or `/=` in `.filter(…)` | the nine field names | `no field is called 'x'` |
| left of `=` in `.sort(…)` | `columns` | `.sort takes one keyword argument, 'columns'` |
| a value, or a list element | `Active` `Inactive` `Empty` `Archive`, or `All`/`Any` applied | `no value is called 'x' — did you mean "x"?` |
| a `.sort(…)` chain element | `None`, or `Asc`/`Desc` applied | `no value is called 'x' — did you mean "x"?` |
| the head of a wrapped kwarg | `not` | as the value row |
| the head of a positional | `raw` | as the value row |
| anywhere in `.columns(…)` | nothing — the stage takes strings | `a column is a string — did you mean "x"?` |

The last five rows of the "otherwise" column are the **did-you-mean-quotes**
diagnostic, and it is the one the case fold makes load-bearing: with case no
longer telling a constructor from a name, an unresolvable bare word is always a
reader who meant the open half.

#### The field roster and its domains

Derived from `metaHome` (`AGENTS.hs`:2526-2534) and the meta table
(`docs/query.md`:327-332). `Empty` is legal on every COLUMN key and on
`planned`, and nowhere else.

| field | `Empty` | other metas | open values | temporal | note |
| --- | --- | --- | --- | --- | --- |
| `state` | ✓ | `Active`, `Inactive` | keyword strings | — | folded; reads through org's brackets |
| `priority` | ✓ | — | `"A"` `"B"` `"C"` | — | `"[#A]"` folds to the same |
| `title` | ✓ | — | any string | — | substring |
| `scheduled` | ✓ | — | date-prefix strings | **✓** | `"2026-08"` is a month |
| `deadline` | ✓ | — | date-prefix strings | **✓** | |
| `tag` | ✓ | `Archive` | tag strings | — | substring of the `:a:b:` cell |
| `planned` | ✓ | — | date-prefix strings | **✓** | either date cell |
| `closed` | ✓ | — | date-prefix strings | **✓** | the CLOSED: stamp; arrives with the flat key, L8 phase 2 |
| `ref` | ✗ | — | id strings | — | the ONE value not case-folded |
| `substring` | ✗ | — | any string | — | free text under a key |
| `sort`, `columns` | — | — | — | — | STAGES, never fields (§7) |
| `view` | — | — | — | — | a shell pragma, expanded ahead of the fetch |

**The temporal column is L8's domain,** and it is the whole of it: a comparison
operator is legal on a field marked there and on no other. `closed` is the tenth
field and it is not a field yet — it lands with the flat key
([the flat half](2026-08-21-datetime-comparisons-in-the-flat-grammar.md), phase
2), and a custom timestamp property joins the same column in that proposal's
phase 3 under its own spelling.

The six sortable columns are `state`, `priority`, `title`, `scheduled`,
`deadline`, `tag` (`Query.hs`:1909-1917), named in a `.sort(…)` chain by key or
by header — `State`, `#`, `Title`, `Scheduled`, `Deadline`, `Tags` — folded. A
CUSTOM column is a legal `.columns(…)` name and an illegal `.sort(…)` one:
custom cells "are not sortable chain keys" (`docs/query.md`:373-374).

#### The judgments

```
   f ∈ fields      v : dom(f)                 f = v : Binding
   ──────────────────────────── KWARG        ────────────────── NEG
        f = v : Binding                       f /= v : Binding


   vᵢ : dom(f)  for every vᵢ                  vᵢ : dom(f) ∪ [dom(f)]  for every vᵢ
   ─────────────────────────── ANY           ──────────────────────────────────── ALL
        Any [v…] : dom(f)                          All [v…] : dom(f)


   e : Binding      All ∉ e                   c ∈ sortable   s ∈ { "c", Asc "c", Desc "c" }
   ─────────────────────────── NOT           ──────────────────────────────────────────── SEG
        not (e) : Binding                                   s : Seg


   f ∈ temporal     d : Date     ⊙ ∈ { <, <=, >, >= }
   ───────────────────────────────────────────────── CMP
                     f ⊙ d : Binding


   sᵢ : Seg  for every sᵢ                     ─────────────── ORDER-NONE
   ───────────────────────── ORDER            None : Chain
       [s…] : Chain
```

`NEG` and `NOT` are the same rule twice: `f /= v ≡ not (f = v)`, and the checker
normalizes the wrapper away. Double negation collapses.

**`CMP` is narrow in two ways of its own,** and each is the flat grammar's: the
field must be temporal, and the right side must be a single `Date` — no list, no
`Any`, no `All`, no `Meta`, because the flat comparison reads ONE literal. There
is no negated operator either ([L8](#l8--datetime-comparisons), rule 4).

`ALL` spreads over ELEMENTS (§5.3); an element of `Any` may not be a list
(§5.1); `NOT`'s second premise is the refusal §5.4 states.

#### Argument order

Positionals precede kwargs. The grammar admits the other order so the checker
can name it: `an argument follows a keyword argument`.

#### Binding collisions

A stage's kwargs are a record, so one slot may not hold two answers. The test is
the flat string's: **two bindings collide when they would compose the same TOKEN
SHAPE — same field, same operator, same sign — with DIFFERENT values.** An exact
duplicate is no collision: it is idempotent and drops, which is
[T7](#the-laws-as-theorems).

`tag = "web", tag = "glance"` collides and is refused with `All` named;
`tag = "web", tag = "web"` drops one; `state = "TODO", state /= "DONE"` stands,
and so does `deadline >= "A", deadline < "B"`
([L8](#l8--datetime-comparisons), rule 5). Before L8 the rule was "a field is
bound once", which is what this refines and which `All` (§5) exists because of.

### L5 — Dynamic semantics

#### The denotation is a function algebra

**A stage is a typed function on the dataframe, and a query is their
composition.** That is the whole denotation, and everything below is its
consequence.

```haskell
filter  :: Pred  -> DF -> DF
sort    :: Order -> DF -> DF
columns :: Sel   -> DF -> DF

⟦.filter(p).sort(a).columns(x)⟧  =  columns x ∘ sort a ∘ filter p
```

The chain reads left to right as the pipeline the additive-filters proposal
already named — `df.filter(⋀ axis-exprs).orderBy(sort:).select(columns:)`
([additive-filters](../done/2026-08-20-additive-filters.md):174) — and the
surface's `.` denotes `∘`, written in the order a reader types it.

#### The composition table

Notation: `⧺` is list append, `ε` the empty chain and the empty selection, `⊤`
the trivial predicate ([additive-filters](../done/2026-08-20-additive-filters.md):106),
and `∘` composition, `f` applied first.

Each stage kind is a MONOID, and the three commute. One row per stage pair,
each grounded in the flat grammar's own pinned behavior:

| `g ∘ f` | equals | grounded in |
| --- | --- | --- |
| `filter p ∘ filter q` | `filter (p ∧ q)` | appending a filter can only intersect ([additive-filters](../done/2026-08-20-additive-filters.md):178-180); the spike's "the chain is honest for `filter`" (README:746) |
| `sort b ∘ sort a` | `sort (a ⧺ b)`, a repeated column keeping its FIRST spelling, direction included | `docs/query.md`:343-345 — `->` is sugar for writing several `sort:` tokens, written order is the chain's order |
| `columns y ∘ columns x` | `columns (x ⧺ y)`, a repeated name keeping its first spelling, case-folded | `docs/query.md`:367-368; `Columns.hs`:18-19 — the tokens' names `concat` and `firstBy` dedupes |
| `sort None` beside any ordering companion | REFUSED, either order | `*none*` "is the whole order and stands alone" (`docs/query.md`:351-352) — the one element that is no monoid member |
| any two stages of DIFFERENT kinds | COMMUTE | "narrowing tokens AND in any order; only `sort:` and `columns:` read their written order" (`docs/query.md`:418-419) |
| `filter ⊤`, `sort ε`, `columns ε` | the IDENTITY | `.filter()`, `.sort()` and `.columns()` each compose nothing |

Each of the three is idempotent — `∧` absorbs, and first-spelling-wins absorbs —
so the product is a commutative, idempotent algebra. That is exactly the
property the IR's sort-and-dedupe quotients by, and T7 is it said for one stage.

**The badge is the composed function.** The spike folded the shaping stages so
the strip never shows two of either, and called it a display rule
(README:749-750, :1189). Under this table it is the algebra's normal form drawn:
one badge per kind IS the composed element of that kind's monoid.
`.sort(a).sort(b)` shows one order badge because `sort b ∘ sort a` IS one order,
and the fold is arithmetic rather than tidying.

#### The normal form, and totality

**Normal form and totality** — [T10](#the-laws-as-theorems). The consequence for
this proposal: the function space is closed under its one combinator, so there
is nothing a second grammar could carry that this one cannot. That is the ground
§1's no-second-asset and the Alternatives' no-second-wire-grammar decisions
already stood on, now semantics rather than taste.

**The wart, stated.** The additive sign lives inside the PREDICATE algebra and
never in the composition algebra. `filter p ∘ filter q` can only INTERSECT,
where `+` is a per-axis UNION that rewrites its axis's expression instead of
appending a stage ([additive-filters](../done/2026-08-20-additive-filters.md):178-184).
So no amount of stage composition reaches it — and that is precisely why the
kwargs hole (§4, [T9](#the-laws-as-theorems)) sits where it does: the chain is
total over stages while the predicate it composes over has a form,
`base ∨ wide`, with no kwargs spelling, and it is the boundary
[per-axis satisfiability](#per-axis-satisfiability) runs inside.

#### The two paths and one IR

Both readers build TERMS — the flat grammar's own `Term` (`Filter.hs`:110-114),
sign, key and value — and one builder writes the IR. The DSL additionally
COMPOSES back to the flat string, which is what reaches the wire.

```
             read_t                build
   DSL ─────────────────► Terms ───────────► IR
    │                       │
    │ compose               │ spell
    ▼                       ▼
   flat ────────────────► Terms ───────────► IR
             read_f                build
```

#### The IR's grammar

```ebnf
ir      = "(" "query" filter order select ")" ;
filter  = "(" "filter" { axis } ")" ;              (* axes SORTED by key *)
axis    = "(" "axis" key term ")" ;
term    = atom | meta | cmp
        | "(" "not" term ")"
        | "(" "and" term term { term } ")"         (* flattened, sorted, deduped *)
        | "(" "or"  term term { term } ")" ;
atom    = "(" "atom" key string ")" ;
meta    = "(" "meta" key word ")" ;
cmp     = "(" "cmp" key ir-cmp-op string ")" ;     (* L8; temporal keys only *)
ir-cmp-op = "lt" | "le" | "gt" | "ge" ;
order   = "(" "order" ( "default" | "none" | { "(" "seg" key dir ")" } ) ")" ;
dir     = "asc" | "desc" ;
select  = "(" "select" ( "default" | { string } ) ")" ;
```

`<` prints `lt`, `<=` prints `le`, `>` prints `gt`, `>=` prints `ge`; the
surface operator and its IR spelling are one-to-one.

Each axis prints the additive proposal's own denotation,
`(P ∪ N ≠ ∅ ∧ base) ∨ wide` (`additive-filters`:109-113). Sorting the axes and
flattening-sorting-deduping the connectives quotients associativity,
commutativity and idempotence away, so **two spellings that MEAN the same print
the same bytes.**

`cmp` is a THIRD leaf beside `atom` and `meta` rather than a shape over them,
and the reason is the flat half's law 6, which [L8](#l8--datetime-comparisons)
rule 4 restates for the surface: the four operators do not pair off under `not`,
so no normalization may rewrite one into another. The builder prints the
operator it was given. A `cmp` and an `atom` on one key never collapse either — the
bare form is the closed interval, an equivalence the LANGUAGE states and the
normal form deliberately does not apply.

```
state:*active* -tag:chore
.filter(state = Active, tag /= "chore")

  ⇓ both readers

(query (filter (axis state (meta state active))
               (axis tag (not (atom tag "chore"))))
       (order default) (select default))
```

#### The compile

Each stage compiles to its own flat separator: a space in `filter`, `->` in
`sort`'s chain, `,` in `columns` — the comma's per-stage reading, the spike's
round 2. A binding compiles to `key:value` or `-key:value`, the key CANONICAL
LOWERCASE; a meta constructor to its starred word; `Any` to `|`-alternatives;
`All` to repeated tokens; a comparison to `key:OPvalue`, the operator written
into the VALUE where the flat grammar reads it
([L8](#l8--datetime-comparisons)). In `.sort(…)` a `Seg` compiles to its resolved key
with `:desc` under `Desc` and nothing under `Asc` or a bare string; `None` and
`[]` both compile to `sort:*none*`. In `.columns(…)` each string compiles as
written, the header spelling preserved, quoted where it carries a separator
(`AGENTS.hs`:2361-2362). `raw "s"` compiles to `s`, its doubled quotes halved.

#### Round-trip

| typed | flat |
| --- | --- |
| `.filter(state = Active)` | `state:*active*` |
| `.filter(STATE = active)`, `.filter(state = ACTIVE)` | `state:*active*` — the fold, T8 |
| `.filter(state = "TODO")` | `state:TODO` |
| `.filter(state = "ACTIVE")` | `state:ACTIVE` — the tree's keyword, told from the meta by the QUOTES |
| `.filter(tag /= "chore")` | `-tag:chore` |
| `.filter(not (tag = "chore"))` | `-tag:chore` — the wrapper normalizes away |
| `.filter(state = ["TODO", "DONE"])`, `… = Any […]` | `state:TODO\|DONE` |
| `.filter(state /= ["TODO", "DONE"])` | `-state:TODO\|DONE` |
| `.filter(tag = All ["web", "glance"])` | `tag:web tag:glance` |
| `.filter(tag = All ["web", ["glance", "docs"]])` | `tag:web tag:glance\|docs` |
| `.filter(deadline < "2026-09")` | `deadline:<2026-09` — L8 |
| `.filter(deadline >= "2026-09-01", deadline < "2026-10-01")` | `deadline:>=2026-09-01 deadline:<2026-10-01` — the range, two tokens on one axis |
| `.filter(not (deadline < "2026-09"))` | `-deadline:<2026-09` — the undated rows among them, and NOT `deadline >= "2026-09"` |
| `.filter(planned = Empty)` | `planned:*empty*` |
| `.filter(tag = Archive)` | `tag:archive` |
| `.filter(ref = "abc123")` | `ref:abc123` — the one value not case-folded |
| `.filter(substring = "milk")`, `.filter("milk")` | `substring:milk` |
| `.filter(tag = "-chore")` | `tag:"-chore"` — a literal, never a sign |
| `.filter("STATE:TODO")` | `substring:STATE:TODO` — the flat free-text reading, said on purpose |
| `.filter(raw "priority:[#A] +priority:[#B]")` | verbatim |
| `.filter(raw "substring:""-x"" +tag:web")` | `substring:"-x" +tag:web` — `raw`'s doubled quote halves |
| `.filter()` | nothing |
| `.sort(columns = ["Deadline"])` | `sort:deadline` — the header in, the key out |
| `.sort(columns = ["deadline"])` | `sort:deadline` — key or header, folded |
| `.sort(columns = ["Deadline", Desc "Title"])` | `sort:deadline->title:desc` |
| `.sort(columns = ["Deadline", Asc "Title"])` | `sort:deadline->title` — `Asc` is spellable, never emitted |
| `.sort(columns = None)`, `.sort(columns = [])` | `sort:*none*` — document order, spellable only this way |
| `.sort()`, no stage | nothing; the default chain stands |
| `.columns("State", "Deadline")` | `columns:State,Deadline` |
| `.columns("owner")` | `columns:owner` — a custom column, the open set's own |
| `.columns("Sprint 3")` | `columns:"Sprint 3"` — a name with a space is quoted, the flat grammar's own rule |
| `.columns()` | nothing; the default six, which `columns:` also gives |

#### The laws, as theorems

Write `read_t` for the typed reader, `read_f` for the flat one, `compose` for
the composer, `render` for the flat-into-surface direction, `IR` for the
builder, and `rows(q)` for what the server serves (`matchesFilter`).

- **T1 · Agreement.** For every corpus pair `(d, f)`:
  `IR(read_t(d)) = IR(read_f(f))`, as BYTES.
- **T2 · Soundness of composition.** For every well-typed `d`:
  `IR(read_t(d)) = IR(read_f(compose(d)))`. The composer preserves meaning.
- **T3 · Conservativity.** `rows(compose(d)) = ⟦d⟧`. Immediate from T2 and the
  fact that the server reads `compose(d)` and nothing else — the DSL adds no
  rows and hides none.
- **T4 · No new power.** For every well-typed `d`, `compose(d)` is a query the
  flat reader ACCEPTS. The surface can spell nothing the flat string cannot;
  this is L0's law and §10's second invariant, and a corollary of
  [T10](#the-laws-as-theorems) — a canonical form cannot carry what its own
  algebra does not generate.
- **T5 · Losslessness of rendering.** For every flat `f`:
  `IR(read_f(compose(render(f)))) = IR(read_f(f))`. Equality is at the IR and
  never at the bytes — normalization moves a header to its key and drops an
  `:asc`, so `/`-editing a query may rewrite its spelling while never changing
  what it serves.
- **T6 · Totality of rendering.** Every flat `f` renders, with `raw` as the
  residue. So `/` never destroys a query the surface cannot say.
- **T7 · The quotient.** `IR(d) = IR(d')` whenever `d'` permutes a stage's
  arguments or repeats one. This is additive-filters' law 1
  (order-independence) and law 4 (idempotence) said for the surface.
- **T8 · Case invariance.** `IR(d) = IR(d')` whenever `d'` differs from `d` only
  in the case of bare words. The fold is a lexical fact with no semantic
  residue.
- **T9 · Non-totality of the pretty fragment.** There is a flat `f` — law 5's
  parting case, `priority:[#A] +priority:[#B]` — with NO `raw`-free DSL
  spelling. Named as a theorem so the hole is a stated fact rather than a
  caveat; §4 is its consequence.
- **T10 · Closure and canonicity.** The function space is generated by the three
  stage constructors and composition, and closed under both; each kind's monoid
  is idempotent and the kinds commute, so every query has a unique normal form
  and `compose` prints it. **The flat `?q=` is the algebra's canonical form,**
  and `IR` is its normalization made checkable. T4 is the corollary — a
  canonical form cannot carry what its own algebra does not generate.

The spike's corpus bites both ways on T1 and T7: drop the sort-and-dedupe and
the order/idempotence pairs go red; conjoin the widening instead of disjoining
it and law 5's agreement pair goes red; let `All` flatten and the intersection
pairs go red; stop `raw` reaching the flat reader and the escape-hatch pairs go
red; stop a constructor normalising to its meta and every meta pair goes red.
All five were run (spike README:990-1002).

### L6 — Diagnostics

**The error model has three tiers, and the flat grammar decides which tier a
form lands in.** The flat reader refuses only the shaping keys — "everything
else that fails to parse is free text; everything half-typed narrows nothing"
(`docs/query.md`:428-432). So:

| tier | when | what the reader sees | the rule |
| --- | --- | --- | --- |
| **error** | type time, while typing | the term is marked, the stage cannot close, nothing composes | the form has no flat image, or the flat reader would 400 |
| **warning** | type time | the term — or every term in the contradiction — is dimmed, and ONE line is spoken; it composes unchanged | the flat string accepts it and it serves nothing |
| **quiet** | compose time | nothing; the term is dropped | the flat reader drops it — a half-typed value, an empty list |

**Every message takes the flat reader's own shape** — `reason: 'the term as it
was written'`, which is `refusedOn`'s (`Filter.hs`:95-101), and where the flat
reader already owns the sentence the DSL reuses it verbatim rather than writing
a second one.

#### Per-axis satisfiability

**The DSL warns where the grammar is merely honest.** Two bindings can be
individually legal, compose to a query the flat reader accepts, and together
name a row that cannot exist. The flat string says so by serving nothing, which
is truthful and silent; the surface says so out loud.

**Rule (a) — required and forbidden.** On any axis, a value both REQUIRED (by
`=` or by an `All` element, the base conjunction) and FORBIDDEN (by `/=` or
`not`) contradicts.

```haskell
.filter(tag = All ["docs", "chore"], tag /= "chore")
   ⇓ composes, and the flat reader accepts it
tag:docs tag:chore -tag:chore          -- serves nothing, truthfully
```

On the three PREFIX keys the rule reaches one step further: a forbidden value
that PREFIXES a required one contradicts too, since everything under
`2026-08-15` is under `2026-08` (`AGENTS.hs`:2370-2371, :2374-2375).

**Rule (b) — two distinct required values.** On a key whose match is the WHOLE
cell, two distinct required values contradict. Reachable only through `All`,
because the plain collision `state = "TODO", state = "DONE"` is refused a tier
up ([binding collisions](#binding-collisions)).

The scope is the matcher's, and `AGENTS.hs`:2366-2375 is the roster:

| matcher | keys | two required values |
| --- | --- | --- |
| `MWhole`, `MExact` | `state`, `priority` | always contradict |
| `MPrefix` | `scheduled`, `deadline`, `planned` | contradict UNLESS one prefixes the other |
| `MInfix` | `title`, `tag`, and `substring` | never contradict — one cell carries both needles |
| the semi-join | `ref` | never — a subtree may link to many targets |

So `deadline = All ["2026-08", "2026-09"]` warns and
`deadline = All ["2026-08", "2026-08-15"]` stays quiet, and
`state = All ["TODO","DONE"]` — the flat `state:TODO state:DONE`, which "is
nothing (one cell cannot be both)" (`docs/query.md`:420-421) — is the rule's
plainest instance.

The tier is WARNING by the tier rule's own test — the flat string accepts it and
it serves nothing. **The diagnostic marks BOTH bindings and speaks ONE line; the
compose and the meaning stand untouched.**

**Where the check runs.** Over the predicate algebra's atoms, PER AXIS, after
normalization — the IR's `axis` term, where `not` has been normalized in and
duplicates deduped ([L5](#the-irs-grammar)). Two consequences follow from that
placement:

- It is inside the PREDICATE algebra, never the composition algebra, so it is a
  property of one axis rather than of the chain.
- **An axis carrying a widening is skipped.** The axis reads
  `(P ∪ N ≠ ∅ ∧ base) ∨ wide`, so a contradictory `base` still serves the `wide`
  rows and the warning would be false. `+` is unreachable from the surface
  except through `raw` ([T9](#the-laws-as-theorems)), so the skip is rare and
  exact rather than a hedge.

#### The refusals, by tier

Every form the checker marks, with its tier and the sentence the reader hears.

| form | tier | message |
| --- | --- | --- |
| `state = Archive` | error | `'Archive' is no value for 'state'` |
| `substring = Empty` | error | `'Empty' is no value for 'substring'` |
| `deadline` bare in a value | error | `no value is called 'deadline' — did you mean "deadline"?` |
| `Deadline` bare in `.columns(…)` | error | `a column is a string — did you mean "Deadline"?` |
| `note = "later"` | error | `no field is called 'note'` |
| `sort = …` inside `.filter(…)` | error | the stage refusal ([§7](#7-the-refusals-new-home)), plus the offer `) .sort(columns = [` |
| `view = "default"` | error | `'view' is a saved view, expanded before the fetch` |
| `.sort(columns = ["owner"])` | error | `no column is called 'owner'` — `Sort.hs`:60-61, verbatim |
| `.sort(columns = [None, "Title"])` | error | `'*none*' is the whole order and stands alone` — `Sort.hs`:70-71, verbatim |
| a direction that is neither | error | `a sort direction is 'asc' or 'desc'` — `Sort.hs`:62-63, verbatim |
| `.sort(columns = […], x = …)` | error | `.sort takes one keyword argument, 'columns'` |
| `tag = "web", tag = "glance"` | error | `'tag' is bound twice — did you mean All?` |
| `deadline < "a", deadline < "b"` | error | `'deadline <' is bound twice` |
| `tag = "web", tag = "web"` | quiet | one drops — idempotence, T7 |
| `title < "x"` | error | `'<' is no operator for 'title'` — [L8](#l8--datetime-comparisons) |
| `deadline < ["a", "b"]`, `deadline < Empty` | error | `a comparison takes one date` |
| `deadline < "later"` | warning | `that is no date — this narrows to nothing` |
| `deadline < ""` | quiet | dropped, as the flat reader drops a bare operator |
| `not (f = All […])` | error | `a negated intersection has no spelling` |
| `Any ["a", ["b","c"]]` | error | `an alternation does not nest — did you mean All?` |
| a positional after a kwarg | error | `an argument follows a keyword argument` |
| `tag = All ["docs","chore"], tag /= "chore"` | warning | `'chore' is required and forbidden — this narrows to nothing` — rule (a), both bindings marked |
| `deadline /= "2026-08", deadline = "2026-08-15"` | warning | the same line — the forbidden value prefixes the required one |
| `state = All ["TODO","DONE"]` | warning | `'state' cannot be two — this narrows to nothing` — rule (b), the whole-cell matcher |
| `deadline = All ["2026-08", "2026-08-15"]` | quiet | satisfiable: one date prefixes the other |
| `title = All ["a", "b"]` | quiet | satisfiable: one cell carries both needles |
| `f = []`, `f = ""` | quiet | dropped, as the flat reader drops a half-typed token |

Two forms the checker never marks, handled outside it:

- **A value opening with a space** — untypable into the slot; it wants `raw`
  (§9).
- **`.sort(…).sort(…)`** — folded to one stage by the composition table's own
  arithmetic ([L5](#the-composition-table)), never refused.

### L7 — Evolution

**Reserved room, and the rule that governs all of it:** a form may join the
language exactly when the flat string can already carry it. The surface never
leads the grammar.

- **New stages.** The call position holds three names. A fourth PRIMITIVE lands
  when a fourth shaping key exists for it to compose to. `view:` is the standing
  candidate and deliberately has no stage: it expands in the shell ahead of the
  fetch (`docs/query.md`:447-449), so a `.view(…)` would compose something the
  wire never sees.
- **New stages that are not primitives.** A USER DEFINITION
  ([L9](#l9--the-context-is-data)) adds a name to the call position without
  adding a key, because it expands to stages that already exist. The rule above
  is untouched: the definition composes exactly what its body composes. This is
  the one route by which the call position grows without the flat grammar
  growing first, and it is sound precisely because it adds no denotation —
  composition is the only combinator it uses
  ([L5](#the-composition-table)).
- **New operators.** `=` and `/=` hold the op position; `=~`, `<`, `>` are the
  obvious next ones and none may land before the flat grammar has the predicate
  underneath. **`<`, `<=`, `>` and `>=` are the first to take that route, and it
  is the route they took:** the predicate is designed into the flat grammar in
  [its own proposal](2026-08-21-datetime-comparisons-in-the-flat-grammar.md), and
  [L8](#l8--datetime-comparisons) is what the surface then owes. `=~` still has
  nothing underneath it and stays unspellable.
- **New fields.** The roster is the flat key set's narrowing half, so a field
  lands when a key does — `closed` with the flat half's phase 2. An OPEN field
  name is the one opening this language has not spelled: `prop.NAME:` arrives in
  that proposal's phase 3, and the bare-position question goes with it
  ([L8, custom timestamp properties](#custom-timestamp-properties)).
- **New constructors.** The nullary roster IS `AGENTS.hs`'s `Meta` sum. A sixth
  meta there gives a sixth constructor here, named by the compiler on the
  Haskell side (closed sums, one equation per constructor, no wildcard —
  `docs/invariants.md`:155-159) and by the divergence table on the renderer's.
- **New kwargs on a stage.** `.sort(…)` binds one field today; a tie-break or a
  locale lands beside it without moving the chain out of its position. That
  room is why `sort` is a kwarg stage at all.
- **Graceful degradation.** A renderer that predates a constructor renders its
  flat token as `raw`, because `raw` is the residue of everything the typed
  reader does not know. An older surface degrades to the flat spelling rather
  than dropping it — T6 is what guarantees this.

**The corpus IS the conformance suite.** A change to the language is a change to
the corpus FIRST: the new form lands as a pair — its DSL spelling beside its
flat image — and the pair is red until both readers agree at the IR. Backwards
compatibility has an exact meaning: every existing pair still prints its bytes.
L3 names the rows that owe a pair; §10 names where the corpus lives.

There is no version on the surface, because there is none on the string.

### L8 — Datetime comparisons

> **DEPENDENCY.** This section is inert until
> [datetime comparisons in the flat grammar](2026-08-21-datetime-comparisons-in-the-flat-grammar.md)
> lands. It composes `deadline:<2026-09`, a string today's flat reader accepts
> and answers with no rows, so shipping L8 first would break
> [T4 · No new power](#the-laws-as-theorems) in the only way T4 can be broken:
> by MEANING more than the string means. **L8 is a phase behind that
> proposal's phase 1**, and its `closed` field is a phase behind that
> proposal's phase 2.

**Origin.** The user: *"the DSL should support datetime comparisons — for
SCHEDULED, DEADLINE, CLOSED, and custom properties where org-mode timestamps are
used (active and inactive forms)."*

#### The surface

```haskell
.filter(deadline >= "2026-09-01", deadline < "2026-10-01")
.filter(scheduled < "2026-09", state = Active)
.filter(not (deadline < "2026-09"))
```

Four operators join `=` and `/=` in the binding position
([L2](#l2--grammar)), lexed longest-first ([L1](#the-tokens)), legal on a
temporal field and refused elsewhere by name ([L4](#the-judgments),
[L6](#l6--diagnostics)). The kwarg record is unchanged in shape: a comparison is
one more BINDING, so the stage, the commas, the `-`/`+` sign keys and the whole
completion ladder read exactly as they read today.

#### The five rules, each one the flat grammar's

1. **A comparison is legal on a temporal field alone** — `scheduled`,
   `deadline`, `planned`, and `closed` when it lands
   ([L4's roster](#the-field-roster-and-its-domains)). `title < "x"` is a type
   error rather than a substring search, because the flat `title:<x` IS a
   substring search and the surface must never spell one thing and mean the
   other.
2. **The right side is one `Date`.** No list, no `Any`, no `All`, no `Meta`: the
   flat comparison reads ONE literal. `deadline < ["a","b"]` is an error, and
   the DISJUNCTION of two comparisons — flat `deadline:<a|<b`, one token with two
   atoms — has no record spelling and reaches the surface as `raw`, which is §4's
   hole in its datetime shape.
   **Amended by what shipped, and OPEN:** the flat half's phase 1 put `*today*`
   in the literal position, so a `Meta` now stands exactly where this rule
   admits none — `deadline:<*today*` is legal flat and rule 2 forbids its typed
   image. `Today` is in [L3](#l3--the-prelude)'s table because the roster law
   ([§0](#0--the-roster-law-which-decides-every-spelling)) makes the word the
   language's; whether the typed spelling is `deadline < Today`, a `Date`-typed
   constructor narrower than `Meta`, or something else is **not decided here**
   ([the flat proposal's "As delivered"](2026-08-21-datetime-comparisons-in-the-flat-grammar.md#as-delivered-phase-1)).
3. **A `Date` is a quoted string, and it is a PREFIX.** `"2026-08"` is a month,
   `"2026-08-0"` the first nine days. The granularity law is the flat half's and
   it is quoted here rather than restated: `<` and `>=` cut at the interval's
   first instant, `<=` and `>` cut at its last, so `deadline < "2026-09"` is
   "before September" and `deadline <= "2026-09"` is "September or earlier".
4. **There is no negated operator** — the flat half's law 6, restated for the
   surface. `/<` is unspellable and `deadline /< d` is a lex error. The negation
   is `not (deadline < "2026-09")`, which composes `-deadline:<2026-09`, and it
   is NOT `deadline >= "2026-09"`. The undated rows are outside every comparison
   and inside its negation, so the two differ on exactly those rows and no
   checker may rewrite one into the other.
5. **The range is two bindings.** `deadline >= "A", deadline < "B"` composes two
   tokens ANDing on one axis, which is what two plain flat tokens already do. It
   binds `deadline` twice, and the collision rule admits it because the
   operators differ ([L4](#binding-collisions)) — `All` keeps its job, and the
   `+` key opens a value into a LIST rather than a second binding (§the keys),
   so §4's hole stays out of reach.

#### What it costs each section

| section | the change |
| --- | --- |
| [L1](#the-tokens) | the punct row gains `<` `<=` `>` `>=`, longest-first like `/=`; a bare `<` is legal where a bare `/` is not |
| [L2](#l2--grammar) | `op` gains `cmp-op`; the grammar admits it anywhere so L4 can diagnose by name |
| [L3](#l3--the-prelude) | NOTHING — an operator is no name |
| [L4](#l4--static-semantics) | `Date` joins the types, the roster gains a temporal column and a tenth field, `CMP` joins the judgments |
| [L5](#l5--dynamic-semantics) | the IR gains the `cmp` leaf and its `ir-cmp-op`; the compile writes the operator into the value; four round-trip rows |
| [L6](#l6--diagnostics) | four rows, one per tier — the wrong field errors, a non-date warns, an empty date is quiet |
| [L7](#l7--evolution) | the "new operators" room is spent, by the route it names |
| [§10](#10-pins-and-gates) | a corpus block, and the bite-back that keeps the flat half's law 6 honest |

#### Custom timestamp properties

The mandate names them and the surface cannot reach them, for a reason that
is the governing law rather than an omission: **no flat key names a property**
(`test/TestFilter.hs`:307-329 pins the key set closed), so no field can. They
arrive with the flat half's phase 3 and its `prop.NAME:` namespace, and the
typed spelling that phase owes is a FIELD whose name is open — the first one in
this language, and a §0 question of its own, since `.filter(prop.due < "2026-09")`
puts a tree's vocabulary in the BARE position the reservation law reserves for
the language. **Open, and deliberately unanswered here: how an open field name
is spelled.** The candidate is a constructor over the open name —
`.filter(Prop "due" < "2026-09")` — which keeps the bare position the language's
and the tree's name quoted. It is settled by the flat half's phase 3 and by a
proposal of its own.

The active/inactive question the mandate raises is answered on the flat side and
inherited whole: a comparison reads a timestamp cell THROUGH org's brackets the
way `priority:` reads through `[#A]`, and the timestamp KIND is not narrowable —
`*active*` and `*inactive*` are `state:`'s words and `metaHome` is a function
from a meta to one home (`AGENTS.hs`:2523-2534). So the prelude gains no
constructor for the kind, here or there.

#### The corpus block

Per [L7](#l7--evolution), L8's pairs, each a DSL spelling beside its flat image
agreeing at the IR:

- the four operators, one pair each: `deadline < "2026-09"` /
  `deadline:<2026-09`, and its three siblings;
- the range as two bindings against two flat tokens;
- `not (deadline < d)` against `-deadline:<d`, **and the parting pair beside
  it** — `not (deadline < d)` against `deadline >= "d"`, whose IRs must DIFFER;
- the bare form against its interval — `deadline = "2026-09"` beside
  `deadline >= "2026-09", deadline <= "2026-09"` — whose IRs also differ, the
  equivalence being the language's law and not the normal form's job;
- one error pair per L6 row, which the corpus carries as a form that does not
  compose.

### L9 — The context is data

**Origin.** The user: *"the DSL is evaluated against a predefined context of
rules and defaults, and that context should be visible and editable."*

**The context is visible and editable, and the design in one line: the
interpreter stays the implementation's, and the context is a binding
environment — data end to end.** Nothing about the evaluator becomes
user-supplied; what becomes user-supplied is the environment it evaluates in.

#### The environment, in three layers

| layer | who closes it | today | reachable from the DSL |
| --- | --- | --- | --- |
| the **prelude** | the language | hardcoded, [L3](#l3--the-prelude) | bare words, always |
| the **tree's vocabulary** | the tree | already on the wire — badge values and the view's column descriptors (§8) | quoted strings, always |
| the **user's definitions** | the user | nothing yet — v2 below | bare words, once v2 lands |

The first two ship with this proposal and are the whole of v1's visibility
story. The third is the new mechanism.

#### v1 — the prelude, rendered

**The L3 table, drawn in-app, read-only.** A reader who has pressed `.` sees
every name the language knows — kind, type, legal positions, flat image. The
pane renders the same array the resolver resolves against, which is
[L3](#l3--the-prelude)'s one-list discipline; `docs/dsl.md` is its prose.

#### v2 — user definitions

**A definition binds a name to a DSL term, and the reference is a stage.**

```org
# .org-glance/config/system.org
#+GLANCE_DSL_DEFINE: mine = .filter(tag = "mine", state = Active)
#+GLANCE_DSL_DEFINE: soon = .filter(deadline < "2026-10") .sort(columns = ["Deadline"])
```

```haskell
.mine() .sort(columns = ["Deadline"])     -- referenced by name, expanded before compose
```

This **generalizes the saved-view mechanism that already ships.** `view:NAME`
stands for a whole query, expands in the shell ahead of the fetch, and lives as
a pragma in the tree's config layer — `#+GLANCE_DEFAULT_FILTER` and its two
siblings, user-editable in Emacs or through the settings sheet, with `P` writing
one back (`docs/query.md`:380-409; `docs/config.md`:40-56). A DSL definition is
that same mechanism with a TYPED body and a STAGE reference instead of a flat
string and a token.

##### The expansion law

Five rules, and four of them are the `view:` law's own:

1. **A definition's body is a pure DSL term** — one stage or a chain of them,
   well-typed under [L4](#l4--static-semantics) in the empty definition
   environment. Nothing else is a body: no flat string, no fragment, no partial
   application.
2. **Expansion is ONE pass, and there is no recursion.** A body may not name
   another definition. This is `view:`'s own rule — "the token expands in the
   shell before the fetch", "the first `view:` in the string is the one read"
   — and the reason is sharper here: expansion runs on every keystroke, and an
   expansion that can recurse is an evaluation that can diverge.
3. **Expansion PRECEDES normalization,** so the IR proof covers defined names:
   `IR(read_t(expand(d))) = IR(read_f(compose(d)))`. Every theorem in
   [L5](#the-laws-as-theorems) holds of `expand(d)` unchanged, which is what
   makes a definition free rather than a second semantics.
4. **An unresolvable name takes the existing diagnostic.** [L4's resolution
   table](#resolution-by-position) already answers the call position with
   `no stage is called 'x'`; with definitions the environment supplies the
   suggestion, so the message names the nearest defined name it does know.
5. **Resolution follows the config layer's own precedence:** within one file the
   LAST pragma line wins, and across layers the FIRST system layer that names a
   definition wins (`docs/config.md`:53-56). One rule for pragmas, not two.

##### Composition is the only combinator a definition needs

A definition is an element of the algebra ([L5](#the-composition-table)), and
naming it and chaining it is composition. So v2 adds **no operator, no scoping
construct, no `let`, no argument-passing** — the composition table is the whole
calculus, and `.mine().sort(…)` folds by the same rows as `.filter(…).sort(…)`.
That is the payoff of stating the denotation as an algebra: the extension
mechanism was already there.

##### The wire, unchanged

Definitions never reach the server. Expansion is client-side and happens before
`compose`, exactly as `view:`'s shell expansion does today, so §1's "the server
never learns the DSL" survives v2 whole. The server sees the flat string and
nothing else, and `?q=` stays the one truth.

##### The corpus gains definition pairs

A pair states its ENVIRONMENT beside the two spellings: the definitions in
force, the chain naming one, and the flat string it must compose to. The
existing bite-backs extend by one — let expansion follow normalization instead
of preceding it, and every definition pair goes red, because a name normalized
before it is expanded is a name the builder never saw.

##### What v2 defers: parameterized definitions

`mine(who) = .filter(tag = who)`, referenced as `.mine("dima")`. **Reserved,
and the room is already in the grammar** — the call's argument list exists, and
`.mine("dima")` parses today under the positional-string production. What it
needs is a second design and is named rather than sketched:

- a substitution semantics (textual into a value position, there being no
  binder to capture);
- a typing rule — the parameter's domain is the domain of the field it lands
  in, which means a definition's type depends on its body's use site;
- a diagnostic for arity, and a corpus block of its own.

None of it is hard and all of it is a second proposal. v2 ships nullary
definitions, and the argument list stays empty until that proposal exists.

##### Where the two mechanisms converge

`view:NAME` is already a definition whose body is a whole query and whose
reference is a flat token. Once v2 lands, the honest direction is that they
become one: `view:default` renders as `.default()`, and the three built-in views
become three built-in definitions in the prelude's own table. That is the
direction, stated so it is not stumbled into — it is not v2's scope, and it
would move `docs/config.md`'s saved-views section and `P`'s write path with it.

#### Phasing, and what belongs to this proposal

- **v1 rides Phase 4** — the prelude pane is a rendering of a list Phase 1
  already ships, so it costs a component and no law.
- **v2 is a proposal of its own,** and this section is its design sketch. It
  needs a pragma, a config-layer reader, an expansion pass, an environment on
  the offer list, and a corpus block; none of that belongs in a proposal whose
  first phase has not landed.

## The decisions

### 1. Where the parser and composer live

**Recommend: upstream, in `../table-view/web/table-view.js`, as one section of
the renderer — the typed reader beside the flat one, one composer between
them.**

The surface IS the box. `openFilter` (`assets/table-view.js`:3201), the chip
strip and `chipUp` (:3809), the completion list and its `.tv-ac-dim` rule
(:1157, :4262), the two keydown ladders (:4358) and the dry accept (:4372) are
all one closure in that file. A composer living in `frontend/glue/` would reach
into the renderer's DOM and its completion state across an asset boundary that
today carries a returned handle (:4666-4718) — the ladder cannot be split across
it.

**The shape to copy is `Glance.Web.Filter`'s.** One scan (`scanQuery`,
`src-web/Glance/Web/Filter.hs`:120), one resolve (`parseFilter`, :156), and the
readers sit over that one parse — `Sort.hs`, `Columns.hs` and `viewAddedIn`
(:73) each refuse through the same sentence, spelled "one sentence for all three
readers" (:95). The client-side module is the same figure with the readers
swapped: one token model, the FLAT reader and the TYPED reader over it, one
composer out. It belongs in the file whose renderer is already "a port term for
term" of that Haskell (`Filter.hs`:1-2), because the agreement being proved is
between two parsers that must live in one place to be one function.

**The sync and pin story.** `make sync-renderer` is a copy with a diff check:
`RENDERER := ../table-view/web/table-view.js` (`Makefile`:34) and
`git diff --no-index` decides and reports at once (`Makefile`:110-118). There is
no version pin, the copy being byte-identical being the pin. So the module lands
UPSTREAM first and arrives here by `make sync-renderer`; a glance-local edit
forks the renderer and the two drift with nothing red. Phase 1 therefore has a
prerequisite the phases below name: the upstream checkout is where the code
goes, and this repo's gate runs against the synced copy.

**The server never learns the DSL.** The composer emits the flat string before
`onFilter` fires; `?q=` carries what it carries today. No Haskell module gains a
stage, and `AGENTS.hs`'s query-language model is untouched because the string is
unchanged.

Rejected alternative: a new `assets/query-dsl.js` beside `table-view.js`, called
through a global. It splits the ladder, it forks the grammar, and `make
sync-renderer` would have nothing to say about it.

### 2. The door swap

**Recommend: `/` keeps its command name and gains one reading — the filter door
and the filter stage are one thing.**

Today the two doors are one mount call with one option:
`focusFilter = () => raiseFilter({ narrow: true })` and
`focusQuery = () => raiseFilter()` (`frontend/glue/50-settings.js`:568-569),
bound at `Keymap.hs`:62 and :65. The renderer's `narrowing` flag is the
SESSION's and clears with the box (`assets/table-view.js`:3184, :3225-3232).

The spike's D and F make `/` the filter STAGE's edit key, and declared the
departure rather than dropping it (`check.mjs`'s `DEPARTS`, spike README:669-675).
The tension with the shipped behavior is real and small: today `/` opens a
narrowed TEXT box; under the chain it opens the `.filter(…)` STAGE. Both edit
the filter half of one query and leave the standing shape alone. So:

- **Fresh ground** — no chain up: `/` opens an empty `.filter(` stage in the
  docked box. That is exactly today's narrow door, one shape different.
- **A standing filter** — `/` reopens the `.filter(…)` badge, dashed in the
  box's own accent, and the commit rewrites THAT badge in place rather than
  appending (spike round 4).
- **`.`** opens the chain at the dot, offering exactly `filter`/`sort`/`columns`.

The migration is one sentence: `focusFilter` stops passing `{narrow: true}` and
starts naming a stage. `raiseFilter`'s fallback for an asset that knows no door
(`50-settings.js`:561-565) stays — an old renderer opens its one box, which is
the whole grammar, which is what that fallback already says.

**What the pins cost.** `TestServe.hs`:628-637 asserts the door texts
`["narrow","narrow"]`, `["narrow"]`, `["whole"]`; those become stage names
(`filter` / `chain`). `test/browser/cases.mjs`:1071 is the two-door case and it
splits across phases 2 and 3 (§10). No key moves, no command is renamed:
`filter-rows` still filters rows and `compose-query` still composes the query.

**The picker does not move.** Its own `/` summons the whole flat door over its
own vocabulary (the done proposal's "What stayed", :86-89), and a picker has no
chain and no shaping half. `cases.mjs`:771, :810, :842, :914 stand untouched.

### 3. The IR

**Recommend: proof-first. The denotational IR is test-side; a promotion has
three criteria and none is met yet.**

Two structures are being confused and must be split:

- **The STAGE MODEL** — tokens grouped by stage, which badge owns which tokens.
  This SHIPS, because §2's in-place rewrite needs it: rewriting the filter badge
  means knowing which of the flat string's tokens that badge composed, and a
  string scan cannot answer it.
- **The DENOTATIONAL IR** — the sorted, flattened, deduped s-expression of the
  additive proposal's denotation. This stays in the test.

The reason is the law: the flat `?q=` is the one truth. A denotational IR held
at runtime is a second representation of the query's MEANING, and a second
representation is a second thing that can be right when the string is wrong. The
corpus is where the two readers are made to agree, and a corpus is a test.

**Promotion criteria, all three together:** (a) a surface feature arrives that
the stage model cannot answer and the denotation can; (b) `raw "…"` closes (§4),
so the IR is total over the surface; (c) the corpus has stood one release with
no divergence between the two readers. Until then the builder is imported by the
test alone and the shipped bundle does not carry it.

### 4. The kwargs hole, and `raw "…"`

Stated formally as [T9](#the-laws-as-theorems) — the pretty fragment is not
total — and lexically as `raw`'s one escape ([L1](#the-tokens)).

**Recommend: `raw "…"` ships, rendered but never offered.**

The hole is law 5's parting case — an axis carrying both a base and a widening,
`priority:[#A] +priority:[#B]`, which one kwarg cannot spell
([the wart](#the-normal-form-and-totality);
[additive-filters](../done/2026-08-20-additive-filters.md):134-137, and :182
names it itself). The sharpening the spike affords: **it is a RENDERING problem,
never a composition problem** — nothing typed inside a `.filter(…)` reaches it
([L4](#binding-collisions)), so it only arrives from outside: the address bar, a
saved view's pragma, a `g`-applied default.

So `raw` earns its place as a renderer:

- The surface **renders** such an axis as `raw "priority:[#A] +priority:[#B]"` —
  the flat string quoted into the typed surface rather than mis-said in it — and
  the IR proves the two readings still agree.
- The surface **parses** it back, so a `/`-edit of a query it cannot say is
  lossless rather than destructive. That is the case that makes `raw`
  non-negotiable: without it, opening the filter stage over such a query and
  committing would silently drop the widening.
- No completion ever **offers** `raw`. A reader who types it gets it; the
  vocabulary does not teach it.

The honest cost is the spike's own open question (README:1184-1186): `raw` is an
admission that the pretty language has a hole shaped exactly like the one
feature the last proposal added. It is honest and it is total, and the
alternative — refusing to open a query the surface cannot say — is worse for the
reader who just pressed `/`.

### 5. The intersection, as law

**Recommend: a bare list is `Any`, `All` is the repeated token, and no other
spelling exists.** Typed as `ANY`, `ALL` and `NOT` in
[L4's judgments](#the-judgments); this is the reading behind them.

1. **A bare list is `Any`, and `Any` is the alternation.**
   `tag = ["web", "glance"]` ≡ `tag = Any ["web", "glance"]` ≡ `tag:web|glance`.
   It does not nest: an element of `Any` may not itself be a list, a nested
   alternation saying nothing a flat one does not, so a reader who wrote one
   meant `All`.
2. **`All` is the only spelling of the repeated token.**
   `tag = All ["web", "glance"]` ≡ `tag:web tag:glance`, today's conjunction
   (`docs/query.md`:420-422).
3. **`All` spreads over ELEMENTS, never over atoms.**
   `All ["web", ["glance", "docs"]]` is two tokens and keeps the inner
   alternation: `tag:web tag:glance|docs`. The spike's round-trip rung caught
   the flattening reading and it is now pinned (README:716-722).
4. **`not (…)` cannot carry an intersection.** `not (tag = All ["web","glance"])`
   is ¬(a ∧ b) = ¬a ∨ ¬b, and no conjunction of negated tokens says that. The
   surface names the refusal rather than composing something else.

Record syntax cannot repeat a field under one operator, which is why the
unchosen reading needed a name at all. `All` is that name and it is the only
one — [L8](#l8--datetime-comparisons)'s refinement of the collision rule
([L4](#binding-collisions)) admits a second binding only under a DIFFERENT
operator, and `All`'s case is two bindings under `=`.

### 6. DEL

**Recommend: neither key moves. `stripLastToken` goes stage-sized and the
existing ladder is unchanged.**

The spike reads `DEL` as double-booked: "the stage eraser and the crumb pop want
the same key in the same state. One of them has to move" (README:860-862),
against `docs/query.md`:83-84 — "`@` on a focused row drills into `ref:ID` behind a
breadcrumb; `DEL` pops back."

The shell already resolves it, and by ORDER rather than by a second key.
`frontend/glue/70-shell.js`:185-199:

```js
filterDrop: (b) => {
  if (clearMarking(named(b, "unmark-all"), false)) return;
  if (!wants(b, "filter tokens", "stripLastToken", "getQuery")) return;
  if (!table.stripLastToken()) { said(b, "no filter"); return; }
  const left = table.getQuery().trim();
  if (!left && crumbing() && trail().length) { … popCrumb … }
```

Three rungs, and the crumb rung is reached only when `stripLastToken` succeeded
AND left the query EMPTY. The states are disjoint by construction, which is what
`Keymap.hs`:67-68 already spells in its help: `unmark all, else drop the
filter's last token`, and `keyHints` says `unmark/drop token/back`
(`Keymap.hs`:187).

So the change is one word inside `stripLastToken` (`assets/table-view.js`:3870):
the unit it takes off becomes the STAGE rather than the token. The ladder above
it does not move, the crumb pop does not move, `popCrumb` (:4666) does not move.

**The consequence to accept:** `DEL` walks the query down stage by stage instead
of token by token, so a filter stage of three kwargs goes in one press instead
of three. That is the stage-sized reading the spike picked (round 4), and it is
what makes `DEL` legible beside a strip that shows one badge per call. The help
words change with it: `Keymap.hs`:68 to "drop the query's last stage" and :187
to `unmark/drop stage/back`.

**Inside the box, `DEL` is the box's own backspace** (spike's DEL-INSIDE rung),
and Backspace over a SUMMONED empty box stays dead — the chips are behind the
box, not in it (`assets/table-view.js`:4378-4386). That rule is unchanged and it
is why the stage eraser has to be the consumer's key rather than the box's.

The picker's `DEL` cases (`cases.mjs`:810, :842) are a different mount and do
not move.

### 7. The refusal's new home

**Recommend: the refusal becomes the STAGE's, and it gains an offer. The
wording stays the user's.**

Today a shaping token typed at `/` is refused, left standing in the box, and
spoken by the shell. `frontend/glue/00-core.js`:309-318:

```js
const shapingKey = (spelling) =>
  String(spelling || "").replace(/^[-+]/, "").split(/[:=]/)[0];
const refused = (spelling) => {
    const note = `${shapingKey(spelling)}: autocomplete restricted, this key belongs to #'compose (kbd ".")`;
```

Pinned byte for byte, twice, at `TestServe.hs`:641-655.

Under the chain the same mistake lands in a new place: `sort` hand-typed inside
`.filter(…)`. The spike leaves it composing and names the shape it would want:
"`refused()` in `00-core.js` names `.` as the other door in words — with a typed
stage it could OPEN the stage instead" (README:1128-1129).

Two shapes were considered:

- **(a) Refuse and name the stage.** The sentence stops naming a KEY to press,
  because the door is already open; it names `.sort(…)`.
- **(b) Refuse and move it.** The composer chains the stage for the reader and
  puts the argument in it.

**Recommend (a), with (b) behind one TAB.** A surface that silently moves what
was typed is a surface that guesses; a surface that OFFERS the move is a
completion. So typing `sort` inside `.filter(` offers exactly one item —
`) .sort(columns = [` — and taking it closes the filter stage and opens the
order one on its slot, dry and final like every other accept.

**The sentence at `00-core.js`:315 stays the user's to word.** What this
proposal fixes is the SHAPE — the refusal names the stage now that the door is
open — and the two `TestServe.hs` pins carry the current bytes. Landing the
shape with the current letter is legal: the sentence still names the right
destination, one indirection out.

### 8. The roster on the wire

**Recommend: hardcode the constructors. The view JSON grows no field, because
the closed/open split is ALREADY on the wire under two names.**

`src-query/Glance/Query.hs`:1956:

```haskell
"state"    -> [ "badges" .= badges palette, "values" .= stateValues ]
```

- `stateValues = [activeMeta, inactiveMeta]` (:2006-2007) — the CLOSED metas,
  already declared by the producer.
- `badges palette` (:2030-2035) — the tree's own keywords, grouped `active` and
  `inactive`, straight out of its `#+TODO:` line.

So `state`'s two rosters already ride separately and the renderer already reads
both. The client needs one rule on top: **a constructor is the grammar's, a
quoted literal is the tree's.**

The constructor roster is spec. `AGENTS.hs`:2501-2521 is titled "The starred
family, and it is total": `data Meta = MActive | MInactive | MEmpty |
MNone`, with `metas = [minBound .. maxBound]` and `isMeta` decided against that
list. Closed sums are matched one equation per constructor with no wildcard
(`docs/invariants.md`:155-159), so a sixth meta is named by the compiler on the
Haskell side and lands in the divergence table on the renderer's — which is the
mechanism the port already has for exactly this.

**Why a wire field is worse.** `*empty*` is legal on all six column keys and on
`planned`; `*none*` only on `sort:`. A wire field
would need one roster per key per column, and `stateValues` covers one of the
seven. Growing the descriptor also moves `test/TestSpec.hs`:984, which pins the
column descriptor's exact key roster
(`["badges","header","key","sortable","type","values"]`) — a pin whose whole job
is to catch extras riding the key.

**What the open half completes from:** the badge values above, plus the distinct
cell values the renderer already computes for the suggestion list
(`assets/table-view.js`:1789, :1848). Both exist; neither is new.

**The amendment strengthens the recommendation.** With column names on the
string side (§0), the closed roster the client hardcodes is exactly the six
metas plus the two directions — eight words, every one of them the language's,
none of them a tree's or a view's. The open half is completed from what the wire
already carries: the `columns` array is the view's own column descriptors, which
`.columns(…)` and `.sort(…)` complete against with no new field. A surface that
spelled columns as constructors would have needed the producer to declare a
roster it deliberately does not close.

### 9. The opened slot's space

**Recommend: the cost is stated in `docs/query.md`'s typed-surface table and
left unfixed in the first landing.**

Completing a field — or typing its `=` — leaves `state = "|"` with the caret
between the quotes, so the reader types the value and never the punctuation
(spike round 7, README:479-486). The first keystroke inside an empty slot spends
the space the slot already inserted, so `state = TODO"` yields `state = "TODO"`.

The cost: **a value that genuinely OPENS with a space cannot be typed into the
slot.** It wants `raw`.

The value stays reachable — quoting is the only way to get a separator into a
value in the flat grammar too (`docs/query.md`:16-19) — and the slot is one path
to it while `raw` is the other. A fix exists (a modifier key, or the slot's
first space being literal once a second character stands) and it buys a case the
corpus does not carry. It is one more reason `raw` ships (§4).

**The amendment widens the cost and shrinks it at once.** Every POSITIONAL
string slot opens the same way — `.columns(` yields `.columns("|")` — so the
rule now reaches the column names too. It shrinks because a column name that
opens with a space is a name no producer emits: builtin headers are `State`,
`#`, `Title`, `Scheduled`, `Deadline`, `Tags` (`docs/query.md`:368-370) and a
custom column is a property-drawer KEY, which org's own parser will not give a
leading space. So the case is reachable only through free text and `title:`,
which is where it was already. `raw` still covers it.

### 10. Pins and gates

**Recommend: the corpus is a node check of its own, and the pins below move with
the phases that move them.**

**Pins that move:**

| pin | what changes |
| --- | --- |
| `test/TestServe.hs`:628-637 | the `doors` texts become stage names (`filter`/`chain`) — §2 |
| `test/TestServe.hs`:641-655 | the refusal echo — it moves with the wording, which is §7's open decision |
| `test/browser/cases.mjs`:1071 | "`/` offers the filter half and refuses shaping, `.` composes the whole" — its `.` half in phase 2, its `/` half in phase 3 |
| `test/browser/cases.mjs`:1200 | the docked-box geometry: the strip's content is badges now; the measurement is the same |
| `src-web/Glance/Web/Keymap.hs`:62-68 | help strings for both doors and for `DEL` ("drop token" → "drop stage") |
| `src-web/Glance/Web/Keymap.hs`:187 | `keyHints`: `unmark/drop token/back` → `unmark/drop stage/back` |

**New browser cases** (the spike's own rung list, README:890-916 and :918-975), in
`test/browser/cases.mjs` under `make browser-check` (`Makefile`:71-82):

- **DOT** — `.` spawns one dot and offers exactly `filter`/`sort`/`columns`.
- **PARENS** — the taken call opens them and the caret lands INSIDE, in DOM
  order and on the screen.
- **CHAIN** — a scripted sequence composes exactly one flat string and `RET`
  applies it: the rows, the order, empties last.
- **DRY** — an accept lands bare with the offers closed, and the next keystroke
  wakes them.
- **SIGNS** — `-` flips `=`↔`/=` and back; `+` opens `["TODO", |]` with the
  caret in the slot; each composes the grammar's own flat string.
- **SLOT** — completing the field yields `state = ""` with the caret between the
  quotes and the offers closed; the offers lead with `Active`; a constructor
  swallows the quotes and a literal keeps them; typing past the closing quote
  steps over it.
- **SLOT-POSITIONAL** — the amendment's own rung: `.columns(` yields
  `.columns("")` with the caret between the quotes and the offers closed; `,`
  opens the next one the same way; the offers over a `.columns(…)` slot carry no
  constructor, and the offers over a `.sort(…)` list slot carry `Asc`/`Desc`,
  each of which swallows the quotes and opens a slot of its own.
- **SIGNATURES** — `.sort(` offers the one kwarg `columns = [|]`;
  `.sort(columns = ["Deadline"])` composes `sort:deadline` (the header in, the
  key out); `Desc "Title"` appends `:desc` and `Asc "Title"` appends nothing;
  `columns = None` and `columns = []` both compose `sort:*none*`; a second kwarg
  and an unsortable column are each refused in the stage's own voice.
- **CASE** — the language pass's own rung: `STATE = active`, `state = ACTIVE`
  and `State = Active` all compose `state:*active*`, and the dry accept displays
  `state = Active` whichever was typed. The quoted half does NOT fold:
  `state = "ACTIVE"` composes `state:ACTIVE` and stays a string.
- **DIAGNOSTICS** — one case per tier ([L6](#l6--diagnostics)): an error marks
  and refuses to close the stage (`state = Archive`); a warning dims and
  composes anyway (`state = All ["TODO","DONE"]`); a quiet drop composes nothing
  and says nothing (`state = ""`). The did-you-mean-quotes message is asserted by
  its text, since it is the message the case fold makes load-bearing.
- **SATISFIABILITY** — three assertions over
  [per-axis satisfiability](#per-axis-satisfiability), and the third is the one
  that matters: `.filter(tag = All ["docs","chore"], tag /= "chore")` marks BOTH
  bindings and speaks one line; the near-miss
  `.filter(deadline = All ["2026-08", "2026-08-15"])` stays quiet, one date
  prefixing the other; and the warned chain **still applies** — `RET` delivers
  `tag:docs tag:chore -tag:chore` unchanged and the table serves what the flat
  reader serves for it. A warning that quietly altered the query would be the
  failure this case exists to catch.
- **SLASH-STAGE / SLASH-FRESH** — `/` over a standing filter reopens that badge
  and rewrites in place; `/` on fresh ground opens an empty filter stage.
- **DEL-STAGE / DEL-INSIDE** — the strip's eraser is stage-sized; inside the box
  `DEL` is the box's own backspace.
- **ESC** — one press cancels the input whole (typed text, menu, a `/`-spawned
  slot alike) and the stage restores byte-identical; the strip untouched. The
  escape is from the edit, never from the menu.
- **COMPARE** — [L8](#l8--datetime-comparisons)'s own rung, and it lands only
  once the flat half has: typing `<` over a temporal field's op position flips
  the operator the way `-` flips `=`↔`/=`; `deadline < "2026-09"` composes
  `deadline:<2026-09`; two bindings compose the range as two tokens; `title < `
  marks and refuses to close the stage; `not (deadline < d)` composes the `-`
  token and never the mirror operator.

**Where the IR corpus lives.** Three homes were weighed:

- `test/TestFilter.hs`, where the denotation already lives (the spike's own
  reading, README:1160-1161). **Rejected** — the typed reader is JavaScript and
  Haskell cannot run it, so `TestFilter` can only ever prove the flat half.
- The browser suite. **Rejected** — it drives a page and a server for what is a
  pure function of two parsers, and `browser-check` is its own sitting that
  skips loudly when node or the browser is missing (`Makefile`:72-78).
- **Recommend a node check of its own,** beside `test/harness.mjs`: a runner
  that imports the renderer's module directly, reads a shared corpus fixture,
  and asserts the two readers print the same bytes. No browser, no server, so it
  can join the gate rather than a sitting. It skips loudly without node, the way
  `check-glue` does (`Makefile`:154-158).

**The corpus IS the language's conformance suite** ([L7](#l7--evolution)), so it
grows with the language rather than beside it. A JSON fixture under
`test/fixtures/`:

| block | what it holds | count |
| --- | --- | --- |
| the spike's own | paired spellings, flat against typed | 27 (32 in `check.mjs`, less the five case pairs now counted in the case-fold block) |
| render-and-read-back | flat queries rendered INTO the surface and read back — the `/`-edit's path, `raw` included | 7 |
| the parting pairs | spellings whose semantics DIFFER and whose IRs must differ with them | 6 |
| the amendment's | the direction trio (`Desc "Title"` / `Asc "Title"` / bare), the header-in-key-out normalization, `columns = None` beside `columns = []`, `.columns(…)` against `columns:` | ~12 |
| **the prelude's** | **one pair per [L3](#l3--the-prelude) row that lacks one** — `Any` spelled out, `Asc`, and the three stage functions applied empty | **~5** |
| **the case fold's** | **T8: variants differing only in the case of bare words print one IR** — `STATE = active`, `state = ACTIVE`, `.FILTER(…)`, `Desc` against `desc` | **~6** |
| **[satisfiability](#per-axis-satisfiability)'s** | **one warned pair whose IR is UNCHANGED by the warning, one satisfiable near-miss that stays quiet, one prefix pair per direction of rule (a)** — the corpus asserts the IR, so a check that altered the term instead of annotating it goes red | **~4** |
| **[L8](#l8--datetime-comparisons)'s** | **the four operators, the two-binding range, the two PARTING pairs (a negated comparison against its mirror, the bare form against its interval), one form per L6 row** | **~12, and only once the flat half has landed** |

`test/TestFilter.hs` gains one group reading the same fixture's FLAT halves
through `parseFilter`/`matchesFilter`, so the pairs that mean the same serve the
same rows on the server too. That makes the fixture the shared truth across the
port, which is what `Filter.hs`:1-2 is for.

A rung is owed BOTH ways, as the spike's is: a corpus that cannot go red proves
nothing. The five bite-backs quoted above stand among the spike's thirty-seven
(README:990-1019). Two are new here. The case block breaks the fold's other half
— the spike already mutates the BARE half back to case-sensitive
(README:996); this one folds the QUOTED half too, so the `Active` / `"ACTIVE"`
pair collapses, which must go red. And L8's: normalize a negated comparison into
its mirror operator and its parting pair collapses, which must go red too.

**The docs a language needs.** `docs/dsl.md` is the user-facing law, one page,
`docs/query.md`'s sibling: the string's law on one page, the surface's on the
other, cross-linked both ways. It carries L1–L9 in the register `docs/query.md`
uses — tables and sentences, no proposal apparatus — with L8's page held until
its dependency lands, and it is where a reader who has pressed `.` goes. **Its
placement is Phase 1, not Phase 4;** see the phasing for why.

**Invariants that arrive** (`docs/invariants.md`, the Shape section at :146):

- **The surface composes the flat query and nothing else composes it.**
- **A stage the flat string cannot carry must not be composable.**
- **The two readers print one normal form.**
- **A bare word is the language's and a quoted string is the tree's** — the
  reservation law, and the reason the case fold costs nothing.

**The battery is unchanged otherwise.** The wire, the store, the walk, the write
path, every Haskell reader, `AGENTS.hs`'s query-language model, the parity
divergence table's existing rows, `make interop`, `make elm-test`. `?q=` already
carries the string, and that is the point.

## Phasing

Every phase lands green on `make test` and `make browser-check`.

**[L8](#l8--datetime-comparisons) is outside this ladder and gated on
[the flat half](2026-08-21-datetime-comparisons-in-the-flat-grammar.md)** — its
own DEPENDENCY block states the gate and the order. Its shape is Phase 1's: a
lexer row, a production, a typing rule, an IR leaf, a corpus block, no pixel.

### Phase 1 — the language, the module and the proof, behind no UI

`docs/dsl.md`, the typed reader, the composer and the normal-form builder; the
corpus fixture and the node runner. No key changes, no pixel changes, nothing on
the page can reach the new code.

**`docs/dsl.md` lands HERE rather than in Phase 4, and the call is deliberate.**
The language is defined before it is implemented — that is what the section
above is — so the reference page is the parser's SPECIFICATION and the corpus is
generated against it. A language reference written after the surface ships
documents whatever got built. Phase 4 then adds the cross-links, the
`docs/query.md` sibling pointer and the README row, which are the things that
can only be written once the surface exists.

- **LOC:** ~490 renderer (the flat grammar and the completion machinery are
  already there and are reused), ~120 runner, ~185 fixture, ~350 lines of
  `docs/dsl.md`. Against `assets/table-view.js`'s 4552 lines that is ~11%
  growth.
- **What the amendments cost:** ~80 lines of code — ~55 for the args/kwargs and
  language passes, plus ~25 for per-axis satisfiability — inside the estimate
  above rather than beside it. The args/kwargs pass: a positional-arg path in the
  stage parser (~15), the `seg` reader with its two direction constructors and
  the header-to-key normalization (~20), the `columns = []` → `None` fold (~5).
  The language pass: the fold itself is one `toLowerCase` at the word boundary
  plus a canonicalizing printer (~10), and `raw`'s doubled quote is ~5.
  [Per-axis satisfiability](#per-axis-satisfiability) is ~25 — it walks the IR's
  axis terms, which Phase 1 already builds, and the matcher roster it scopes by
  is a six-row table. The composer is unchanged in shape, because `sort:` and
  `columns:` were already two separate emitters, and the check ANNOTATES rather
  than rewriting, so nothing downstream of it moves.
- **Risk: low.** The code is small and the only gate that moves is the new one.
  Two honest risks. The SYNC: the code lands in `../table-view` and arrives by `make
  sync-renderer` (`Makefile`:110-118), so between the upstream landing and the
  copy the two checkouts differ and only `git diff --no-index` says so. And the
  SPEC: a reference page written ahead of the surface can be wrong about the
  surface, which is why the corpus is generated against the page and the page
  cites the corpus — a divergence is a red rung rather than a stale sentence.

### Phase 2 — the `.` door swaps surface, behind the existing dock

`openFilter` gains a chain shape; the strip draws one badge per call; the
completion vocabulary goes per-stage; the sign keys and the quoted slot land;
the two keydown ladders learn the second shape.

- **LOC:** ~740 renderer, ~40 glue — the args/kwargs pass adds ~20 for the
  positional slot (the same opened-quote machinery pointed at a second
  position), and the language pass ~20 for the canonicalizing accept and the
  three diagnostic tiers on screen. Cases DOT, PARENS, CHAIN, DRY, SIGNS, SLOT,
  SLOT-POSITIONAL, SIGNATURES, CASE, DIAGNOSTICS, SATISFIABILITY, ESC.
- **Moves:** `cases.mjs`:1071's `.` half.
- **Risk: high.** It is concentrated: `assets/table-view.js`:4358 is the
  keydown ladder and :4372 is `const finished = taken.full;` — the branch that
  decides an accept's finality, which must key on the caret (an accept leaving
  the caret inside what it wrote re-offers; spike round 11 records the
  regression a keystroke-keyed reading produced, and its SLOT rung pins both
  routes apart). The datetime landing already moved that decision onto the
  ITEM — it read `taken.full || ac.stage === "value"`, and an offer that merely
  OPENS a token now carries `full: false` — which is the shape a constructor
  opening a call wants. `summoned` (:1668), `filterWrap` (:1952) and the
  `tv-typing` class (:3208) now hold two shapes, and a chain is no `<input>`.
  The mitigation is that the dock, the veil, the strip hues and the commit-only
  delivery (:3881-3894) are all unchanged and already pinned by
  `cases.mjs`:1200.

### Phase 3 — `/` edits the stage, `DEL` erases one

`focusFilter` names a stage (`50-settings.js`:568); `stripLastToken` goes
stage-sized (`table-view.js`:3870) with `filterDrop`'s ladder unchanged
(`70-shell.js`:185); the refusal takes its stage shape (§7); `stash`/`restore`
(`50-settings.js`:584, :606) carry a stage instead of a `.value`.

- **LOC:** ~150, plus the pin edits. Cases SLASH-STAGE, SLASH-FRESH, DEL-STAGE,
  DEL-INSIDE.
- **Moves:** `TestServe.hs`:628-637, `cases.mjs`:1071's `/` half,
  `Keymap.hs`:62-68 and :187.
- **Risk: medium.** Muscle memory is the whole of it: `/` opened a text box
  yesterday. The stash path is the sharp edge — a remount mid-compose must bring
  the chain back, and today it brings back a string through the common door
  (`50-settings.js`:606-618), which is pinned at `TestServe.hs`:621-629.

### Phase 4 — the docs the surface can only now carry

`docs/dsl.md` landed in Phase 1 as the specification; this phase gives it its
neighbours. `docs/query.md` gains "the chain is a view of the string", the
comma's per-stage reading and the sibling pointer; `docs/dsl.md` gains the
pointer back and the screenshots a shipped surface can finally supply;
`docs/invariants.md` gains the four above; the README's query crib gains a row;
this file moves to `docs/proposals/done/` with an "As delivered" section.

**[L9](#l9--the-context-is-data)'s v1 rides here:** the prelude pane,
[L3](#l3--the-prelude)'s table drawn in-app read-only.

- **LOC:** no code beyond the pane; ~120 doc lines on top of Phase 1's page,
  ~80 for the pane and its one browser case (the pane lists exactly the names
  the resolver knows — the case that keeps the two from drifting).
- **Risk: low.** Nothing on the page's existing paths moves.

## What this deliberately does not do

- **It does not replace the flat grammar.** `?q=` is unchanged, byte for byte.
  Every query that works today works after, and the address bar still carries a
  string a reader can share.
- **It does not touch the server.** `Filter.hs`, `Sort.hs`, `Columns.hs`,
  `Routes.hs`, the store, the walk, the write path: none learns a stage. The
  composer emits flat before `onFilter` fires.
- **It does not move the picker.** The picker's `/` still summons the flat box
  over its own vocabulary (done proposal, :86-89). A picker has no shaping half
  and no chain.
- **It does not change `AGENTS.hs`'s query-language model,** because the string
  is unchanged.
- **It does not answer the coarse-pointer path.** The spike's last open question
  stands: a tap has no `.`, no `/` and no `DEL`, and the click handler at
  `50-settings.js`:573-579 still opens the filter half. That is a proposal of its
  own.
- **It does not settle whether the strip keeps token chips.** D and F say one
  badge per call, which makes the chip's `×` and the coarse tap stage-sized too.
  Phase 2 draws badges; the single-token gesture is deferred with the pointer
  path.

## Alternatives considered

- **The flat box stays** — the spike's own control, `a-control.html`. Rejected:
  the user picked the DSL after the spike, and the control fails the mandate by
  construction — a dot in it is a character, and the dropdown lists `state:`
  beside `sort:` in one flat run, which is precisely the confusion the mandate
  is about (spike README:80-84).
- **A Python-kwargs surface** — `filter(state="TODO", tag__ne="chore")`.
  Rejected by the user for Haskell (spike round 6, README:477-478). What the
  choice costs is in the corners and is accepted: `=` beside `/=` is two
  languages, record binding beside Prelude inequality. The fully consistent
  alternatives — `==`/`/=` throughout, or `=`/`not (…)` throughout — are both
  spellable, and the surface accepts `not (…)`, so the second is reachable. The
  mix reads as a comprehension's guard list where the commas are `&&`, and it is
  the picked one (spike README:726-731).
- **ALL-CAPS canonical display** — `.filter(state = ACTIVE)`, the starred metas
  shouted the way org shouts its keywords. Rejected on the collision: a tree may
  define a TODO keyword literally spelled `ACTIVE`, and under ALL-CAPS the
  constructor and the keyword differ by two quote marks alone — `ACTIVE` beside
  `"ACTIVE"` — which is exactly the pair a reader skims past. `Active` beside
  `"ACTIVE"` differs in shape as well as in punctuation. The case fold means
  either display is TYPEABLE either way, so this is a rendering choice and
  nothing else, and it is the one the user may flip without touching a law
  ([L1, canonical display](#canonical-display)).
- **A case-sensitive DSL** — constructors capitalized by requirement, keys
  lowercase by requirement, the way Haskell does it. Superseded by the user's
  fold directive. What the fold costs is one thing and it is stated: the
  constructor/string distinction can no longer ride the case, so it rides the
  QUOTES alone, and an unresolvable bare word needs the did-you-mean-quotes
  message to be a good one ([L4](#resolution-by-position)).
- **Escapes in the string literal** — `\"` and `\\`. Rejected: no flat VALUE can
  contain a quote (`docs/query.md`:16-19), so an escape would spell a value the
  string cannot carry, which the governing law forbids. `raw`'s doubled quote is
  the one admissible exception and the reason is exact — its content is query
  TEXT, re-lexed by the flat reader ([L1](#the-tokens)).
- **The DSL as a second wire grammar** — the server learns `.filter(…)`.
  Rejected: one truth. Two grammars on the wire is two parsers to keep in
  agreement forever, where one composer is one agreement checked by a corpus.
  And a chain is no URL a reader shares.
- **GHC as the interpreter** — ship the compiler, evaluate the chain as real
  Haskell against a real dataframe, and let the user's definitions
  ([L9](#l9--the-context-is-data)) be real functions. It is the honest maximal
  reading of "the DSL is a function composition", and it is rejected on four
  grounds, each of which is a price paid:
  1. **The binary is not one binary.** It ships desktop (`src-desktop-native`,
     `cabal.project.native`) and targets wasm (`cabal.project.wasm`, `make
     wasm-spike`). A compiler inside it is a package database and a filesystem
     in both, and wasm has neither.
  2. **Cold eval is seconds against a server that answers in 0.09 s** — the
     tree is walked once at startup into an in-memory store and a request costs
     an encode (`docs/plan-org-console-web.md`:156, :1040). A query surface that
     re-narrows as it is typed cannot spend a compile per keystroke.
  3. **Sandboxing.** Evaluating user-authored Haskell is arbitrary code
     execution in the process that owns the write path, whose whole discipline —
     one door, drift locks, one writer per file (`docs/invariants.md`, the write
     path) — assumes the process is the only writer.
  4. **The browser still needs its own reader.** The surface completes,
     canonicalizes and diagnoses AS IT IS TYPED, which is client-side by
     construction. So GHC buys the server a second grammar and leaves the
     client's grammar exactly where it stands — the two-grammar problem kept,
     and the whole price paid for keeping it.

  What the algebra costs instead is nothing: composition is closed
  ([T10](#the-laws-as-theorems)), so a definition is a term in a language that
  already exists, expanded before compose and proved by the same corpus.
- **A glance-local composer beside `table-view.js`.** Rejected under §1: it
  splits the keydown ladder across an asset boundary and forks the renderer with
  nothing red to say so.

## See also

- [datetime comparisons in the flat grammar](2026-08-21-datetime-comparisons-in-the-flat-grammar.md)
  — the predicate [L8](#l8--datetime-comparisons) is the typed surface of, and
  the proposal L8 depends on. It carries the granularity law, the empty-cell
  law, the `closed:` axis and the custom-property phasing.
- [additive-filters](../done/2026-08-20-additive-filters.md) — the denotation
  the normal form writes out, and law 5's parting case that §4 is about.
- [the chain a row hangs on](2026-08-23-the-chain-a-row-hangs-on.md) — the
  bounded closure, which reads this proposal's kwarg law for `depth`, owes the
  IR a fourth leaf beside `atom`/`meta`/`cmp` and its own corpus rows, and gates
  that phase on this one landing.
- [`/` filters, `.` composes the whole expression](../done/2026-08-20-slash-filters-dot-expression.md)
  — the two doors this proposal re-reads as two surfaces.
- [`docs/spikes/2026-08-21-dot-chain-box/README.md`](../../spikes/2026-08-21-dot-chain-box/README.md)
  — the six tabs, the sixteen rounds, the corners and the mechanised check.
- [`docs/query.md`](../../query.md) — the whole law of the string being composed,
  and the sibling `docs/dsl.md` is written against.
- [`docs/config.md`](../../config.md) — the config layer
  [L9](#l9--the-context-is-data)'s definitions would live in, and the saved-view
  pragmas they generalize.
- [`docs/invariants.md`](../../invariants.md) — where the four new rules land.
- `docs/dsl.md` — the language reference this proposal specifies. It does not
  exist yet; it lands in Phase 1, ahead of the parser it specifies.

Inert until reviewed.
