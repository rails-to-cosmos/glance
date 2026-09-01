# Proposal — the chain a row hangs on: `ref:ID..N` walks the edges, bounded

**Status:** proposed · **Date:** 2026-08-23 · **Origin:** user, ruling on the
relation-DSL options — *bounded transitive closure over the reference graph,
"everything transitively blocking X", picked for a proposal of its own while the
single-hop predicates ship separately.*

## What this builds on, and does not re-decide

The **single-hop delivery** is the sibling, and every spelling it carries is
settled there rather than here:

| the sibling delivers | reads |
|---|---|
| `ref:ID` | rows whose subtree points AT `ID` — shipped today (`docs/query.md`:80-84) |
| `from:ID` | the reverse: rows `ID` points at (`docs/query.md`:85-89) |
| `ref:ID?kind=SLUG`, `from:ID?kind=SLUG` | the kind test, rhyming with the edge's own file spelling `glance:ID?kind=SLUG` (`docs/query.md`:114-134) |
| `ref:*any*`, `from:*any*` | the existence metas, starred like the rest of the family (`docs/query.md`:136-165) |

This proposal adds ONE thing to that set: a **depth**. It re-designs none of the
four, and every law below reads the sibling's keys as given.

Under both sits the peer's model, which glance reads and never owns: org-glance
writes a typed edge as `glance:ID?kind=SLUG`, dedups on the `(target, kind)`
pair, and slugs a kind on encode and on read
([`../partial/2026-08-12-relations.md`](../partial/2026-08-12-relations.md):95-116;
`AGENTS.hs`:2916-2928). The edges are the peer's; the rows are glance's; a
closure walks the first and answers with the second.

## The law in one line

A `ref:` value may carry a **hop bound** after `..`, and the token then serves
the rows that reach the target in **1 to N hops** along one relation.

```
ref:B..3
```

serves every row from which `B` is reachable in at most three references —
`ref:B`'s answer, iterated three times, with the target itself never in it. The
bound is REQUIRED and capped: the walk a URL can ask for is one whose cost the
reader can see.

## Grammar

### The mark, weighed against the grammar's own laws

Three candidates were put up. What matters is what today's parser already does
with each, since the flat string is the one truth and a new form may not quietly
reclaim an old one.

| candidate | what today's parse does | verdict |
|---|---|---|
| `ref+:ID` | `splitKey` breaks at the FIRST `:` or `=` (`Filter.hs`:245-249), so the key is `ref+`; `fieldOf` (`Filter.hs`:473) does not know it, and the resolver's fallthrough makes the whole token FREE TEXT (`Filter.hs`:184-193). **Live ground** — a needle searched in `hrSearch` today. | refused |
| `ref:ID*N` | **Dead ground.** `metaOf` needs BOTH ends starred (`Filter.hs`:231-233), so `ID*3` is no meta; it reaches `feRef` unresolved and the atom is `const False` (`Filter.hs`:621-624). | refused |
| `ref:ID..N` | **Dead ground on this axis.** `..` is `rangeMark` (`Filter.hs`:273-276), but `stampOf` is reached only under `stamped key` (`Filter.hs`:501-502, :561-563), and `ref` is not a stamped key; the value passes whole to `feRef` and fails to resolve. | **PICKED** |

**Why `ref+:` loses.** The operator would BE the key, so the token has nowhere
to put a bound — `ref+:ID` is unbounded by construction and a cap could only be
a hidden default, which is the one thing the flat-URL law's descendant forbids
(below). Two smaller strikes: `+` is the additive sign's own character in the
token's first position, so `+ref+:ID` is legal and puts one glyph in two jobs
inside one token ([additive-filters](../done/2026-08-20-additive-filters.md):26-28);
and every direction and variation would cost another entry in `fieldOf`'s
roster.

**Why `ref:ID*N` loses.** The parse stands; the collision is in the READING.
`*` is the starred family's glyph (`*empty*`, `*today*`, `*active*`), and a leading `*` is
`refTargetOf`'s own title branch (`Query.hs`:611). A `?q=` is also typed at a
shell often enough that a bare `*` globbing against the working directory is a
real cost.

**Why `ref:ID..N` wins.** The mark already means *a closed interval* in this
grammar, and a bounded closure IS one — the interval of hops `1..N`. The mark
takes its own meaning on a new measure rather than a second meaning, and the
reading a reader must learn is one sentence. It leaves the `?` free for the
sibling's kind, so the value reads with exactly `refTargetOf`'s cut order: the
`?` first, the anchor behind it (`Query.hs`:606-610).

Two shapes the house has floated before, recorded so they are not re-floated:
`ref:ID/2` ([`../partial/2026-08-12-relations.md`](../partial/2026-08-12-relations.md):367-368)
— dead ground too, and `/` is the shell's own filter door plus a path
separator, and a fallback row id IS a path (`Query.hs`:1167-1173); and
`ref:ID<=3`, which reads well beside the comparison family but puts a
value-OPENING operator (`operatorIn`, `Filter.hs`:303-305) in an infix position,
paying the same forking price as `..` for no gain.

**The price of `..`, stated.** On a timestamp key `A..B` has same-sort literals
at both ends; on the ref axis the left end is the ANCHOR and the right end is a
count. The two readings never meet in code — `stampOf` is unreachable from the
ref key — so the fork is in the reader's head, and it costs one sentence in
`docs/query.md`.

### The cut, precisely

The value is read in two cuts, in the link target's own order:

```
ref:<anchor>..<N>?kind=<slug>
     └──────┬──────┘  └──┬──┘
       the anchor half   the sibling's kind half
```

1. **The `?` cut is the sibling's** — `kindCut`, the link target's own reader
   (`Query.hs`:626-628), reached from `refTargetOf` (`:606-610`) and from the
   filter's `anchorIn` (`Filter.hs`:633-636) alike, with the kind slugged by
   `kindSlug` (`Query.hs`:644-645). The cut is taken **only where a `kind=`
   comes out of it**, so an anchor carrying a `?` that declares no kind stays
   whole — which is what leaves `ref:ID..3` unaffected by the `?` reading.
2. **The `..` cut is read FROM THE END** — the right-most `..` whose tail is a
   NON-EMPTY DECIMAL RUN, and only then. Anything else leaves the value whole.
   The discipline is `shiftIn`'s, which reads from the end for the same reason
   ("*what keeps ISO's own separator out of the sign's reach*",
   `Filter.hs`:316-319).

The cut is SYNTACTIC and consults no store, which is what keeps the IR pure —
both readers must build the same bytes over any tree
([the typed DSL](2026-08-21-the-typed-dsl-behind-the-dot-door.md):663-678).

**What that costs, exactly one thing.** A row id is `ORG_GLANCE_ID`, else
`FILE#K` (`Query.hs`:1167-1173). A fallback id always ends `#` and digits, so
the tail after its last `..` always carries the `#` and is never a bare decimal
run — the cut cannot shadow one. An org-glance id is uuid-shaped and carries no
`.`. The single casualty is a HAND-WRITTEN `ORG_GLANCE_ID` ending in `..` and
digits, and there is no escape: inner quotes strip and protect spaces alone
(`Filter.hs`:154-157), so `ref:"ID..3"` reads identically to `ref:ID..3`. That
reader renames the id.

### The depth is required, and capped

- **Required.** The bare `ref:ID` stays exactly one hop, so the closure form is
  the one that carries a number. `ref:ID..` fails the decimal-run test, leaves
  the anchor `ID..`, resolves to no row and serves nothing — the shipped
  unknown-id law (`Filter.hs`:621-624, `docs/query.md`:91-92), and no new rule.
- **`N = 0` serves nothing.** The closure at depth 0 is the anchor alone, and
  the anchor is never served (law 3).
- **Capped, and above the cap serves nothing.** A narrowing token never 400s —
  *"an unknown id matches nothing and does NOT 400, since this is a filter
  rather than a command"* (`docs/plan-org-console-web.md`:1370-1372). Silent
  CLAMPING is worse than either: it serves MORE rows than were asked, and more
  is the one direction a reader cannot check (variant g's own line,
  `spikes/2026-08-21-dot-chain-box/g-sql.html`:24-26). So a depth above
  the cap serves nothing, the way `state:TOD` does, and the typed surface warns
  where it can.
- **The number.** Recommend **8**, pending one census pass for the longest chain
  in `~/sync` — the same script that measured 3524 edges over 10452 rows can
  answer it (`../partial/2026-08-12-relations.md`:60-74). The cap has to sit
  well under `|V|`: by law 7, a cap at `|V|-1` IS the unbounded walk.

**Why any cap at all** — the flat-URL law's own descendant. A query is a string
a human types into a URL, and the language is kept flat so its cost stays
readable ([additive-filters](../done/2026-08-20-additive-filters.md):184-187).
An unbounded walk asks a store the reader has not sized for work the reader
cannot see, so `ref:ID..*` is refused and the bound is what makes the refusal
real.

### Direction: both, in one delivery

`ref:` and `from:` are one relation read from two ends, and the closure follows:

- `ref:ID..N` — rows that REACH `ID` within N hops.
- `from:ID..N` — rows `ID` reaches within N hops.

**Ship both together.** Which one a use case wants is decided by which row wrote
the edge, and the reader does not control that: with `blocked-by` written on the
blocked row, "everything transitively blocking X" is `from:X..N`; with `blocks`
written on the blocker, the same question is `ref:X..N`. Shipping one direction
would make the answer depend on an authoring convention nobody has pinned. The
cost is small — one BFS with the adjacency function flipped, and the sibling
already delivers the key.

### Kinds along the chain: homogeneous when named, any when bare

- `ref:ID..3` — **any kind per hop**, a plain mention (`refKind = Nothing`)
  included. The relation is "points at, however".
- `ref:ID..3?kind=blocked-by` — **every hop carries that kind.**

The ground is one law rather than two: **the closure is the closure of ONE
relation, and the token names one.** A heterogeneous walk that ran `blocked-by`
into `see-also` and back would compute a set with no name a reader could say. It
also keeps the algebra honest — bare is `R⁺` where `R` is the union of every
kind, named is `R_k⁺`, and both are the same operator over a different relation.

The slug is the peer's, unchanged: a hand-typed `Blocked By` and a written
`blocked-by` are one kind (`AGENTS.hs`:2923-2928).

## The graph this is worked on

```
     (E) ──see-also──► (D) ──blocked-by──┐
                                         ▼
                                        (B) ──blocked-by──► (C)
                                         ▲                   │
                                         │ blocked-by        │ blocked-by
                                         │                   ▼
     (F) ──see-also───────────────────► (A) ◄────────────────┘
```

Six rows, six edges, and a cycle `A → B → C → A`:

| edge | kind |
|---|---|
| A → B | `blocked-by` |
| B → C | `blocked-by` |
| C → A | `blocked-by` |
| D → B | `blocked-by` |
| E → D | `see-also` |
| F → A | `see-also` |

Read an edge as its author wrote it: A's subtree says *blocked-by B*.

## Worked examples

| query | serves | why |
|---|---|---|
| `ref:B` | A, D | shipped, one hop |
| `ref:B..1` | A, D | law 1 — the bare form IS depth 1 |
| `ref:B..2` | A, C, D, E, F | A and D at one hop; C and F reach A, E reaches D |
| `ref:B..3` | A, C, D, E, F | B rejoins at 3 through C → A → B and is the anchor — law 3 |
| `ref:B..8` | A, C, D, E, F | the walk exhausts at 3 — its level-3 frontier yields the anchor alone, so deeper bounds add nothing |
| `ref:B..2?kind=blocked-by` | A, C, D | E's and F's hops are `see-also` |
| `from:A..3?kind=blocked-by` | B, C | **everything transitively blocking A** |
| `ref:C..3?kind=blocked-by` | A, B, D | everything transitively blocked BY C |
| `-ref:B..2` | B | the rows the closure does not serve, the anchor among them |
| `ref:B..2 tag:work` | those of A, C, D, E, F tagged `work` | the axis law is untouched |
| `ref:B..2 +ref:C..1` | all six | one axis, the additive law: `base ∨ wide` |
| `ref:B..2\|C..1` | all six | `atomsUnder` splits on `\|` first — two closure atoms |

## Formal semantics

### The edge relation

Fix a store's rows `V`. An edge is a row's own reference, resolved in its own
namespace — `refTest`'s rule, unchanged (`AGENTS.hs`:2589-2603,
`Filter.hs`:597-624):

```
a → b        ⟺  some l ∈ hrLinks(a) names b
a →ₖ b       ⟺  such an l with kindSlug(refKind l) = k
```

Two relations follow: `R = { (a,b) | a → b }` and `R_k = { (a,b) | a →ₖ b }`,
with `R_k ⊆ R`.

### The bounded closure

For a relation `S` and `N ≥ 1`, with `Sⁱ` the i-fold composition:

```
S^(1..N)  =  ⋃ᵢ₌₁..ₙ Sⁱ
```

### Denotation

Let `t = feRef(anchor)`, the row the store resolves; an unresolvable anchor
gives `⊥`, the shipped law. Write `S` for `R_k` when the token names a kind and
`R` when it does not.

```
⟦ref:a..N?kind=k⟧(r)   =  r ≠ t  ∧  (r, t) ∈ S^(1..N)
⟦from:a..N?kind=k⟧(r)  =  r ≠ t  ∧  (t, r) ∈ S^(1..N)
```

The token is ONE ATOM on its key's axis. Sign, alternatives and the axis
conjunction read exactly as they do for `ref:` today
([additive-filters](../done/2026-08-20-additive-filters.md):106-113) — this
proposal touches the atom and nothing above it.

### Laws

1. **Conservativity.** `ref:t..1 ≡ ref:t`, since `S^(1..1) = S`. A query with no
   `..` means exactly what it means today.
2. **Monotonicity.** `⟦ref:t..N⟧ ⊆ ⟦ref:t..N+1⟧` — a deeper bound only adds
   rows.
3. **The identity hop.** `t ∉ ⟦ref:t..N⟧` for every `N ≥ 1`. This is `ref:`'s
   own *a row is not its own reference* extended past one hop, and it is a
   REMOVAL rather than an absence: in the cycle `A → B → C → A`, `A` reaches
   itself at depth 3 and is still refused.
4. **Termination.** The visited set plus the cap. The anchor is seeded into
   `visited`, so it is never re-expanded and never emitted (one rule for law 3
   and for cycles at once); every frontier is a subset of `V \ visited`, so at
   most `|V|` rows are ever expanded, and the cap stops after N levels whatever
   the graph does.
5. **Kind restriction is downward.** `⟦ref:t..N?kind=k⟧ ⊆ ⟦ref:t..N⟧`, since
   `R_k ⊆ R`.
6. **Direction duality.** `r ∈ ⟦from:t..N⟧ ⟺ t ∈ ⟦ref:r..N⟧`. Two keys, one
   relation, two ends.
7. **Saturation.** For `N ≥ |V|-1`, `S^(1..N) = S⁺`. So a cap at or above
   `|V|-1` IS the unbounded walk, which is why the cap must sit well below it.
8. **Composition with the axis law.** The closure adds no axis and no
   combinator: `+ref:t..N` widens the ref axis, `-ref:t..N` inverts inside its
   conjunction, `ref:a..2|b..3` is two atoms ORed, and axes still AND.

### Derivations

`ref:B..2` on the drawn graph:

```
level 0   visited = {B}                      frontier = {B}
level 1   in(B)   = {A, D}                   frontier = {A, D}
level 2   in(A)   = {C, F}   in(D) = {E}     frontier = {C, F, E}
          cap reached
served    {A, D, C, F, E}                    (visited minus the anchor)
```

`from:A..3?kind=blocked-by`:

```
level 0   visited = {A}
level 1   out_bb(A) = {B}
level 2   out_bb(B) = {C}
level 3   out_bb(C) = {A}  — in visited, dropped (law 4 doing law 3's work)
served    {B, C}
```

### The relational reading

[additive-filters](../done/2026-08-20-additive-filters.md):168-187 reads the
language as one dataframe pipeline and names `ref:` as *"the one key that is a
semi-join underneath, against the target's spellings rather than the row's own
cells."* The closure is that semi-join with its right-hand side computed by
iteration:

```
ref:B      ≡  df.semijoin(spellings(B))
ref:B..N   ≡  df.semijoin(close(B, N))
```

One shape, one axis, one row set — the join stays out. What a join would add is
a second row set's COLUMNS in reach; `close` yields ids and nothing else, so the
composition table is the one already pinned.

## Evaluation

### Where it computes

Once per request, never per row — `matchesFilter` compiles ahead of the rows
(`Filter.hs`:458-461) and this rides that. Today the compile-time half of `ref:`
is the target's spellings and the per-row half is a scan of that row's
`hrLinks`. The closure moves the WHOLE join to compile time and leaves a set
membership per row:

```haskell
-- | Everything reachable from T within N hops, T itself excluded.  STEP is the
-- adjacency the direction picked; the visited set is what terminates.
reachable :: (Text -> [Text]) -> Int -> Text -> Set Text
reachable step n t = Set.delete t (go (Set.singleton t) [t] n)
  where
    go seen _        0 = seen
    go seen frontier k =
      let fresh = Set.fromList [ b | a <- frontier, b <- step a
                                   , not (Set.member b seen) ]
      in if Set.null fresh
           then seen
           else go (Set.union seen fresh) (Set.toList fresh) (k - 1)
```

and the atom is then `\r -> Set.member (hrId r) ids`. A pleasant consequence:
the closure's PER-ROW cost is lower than `ref:`'s — one hash lookup against a
scan of `hrLinks` — and the whole cost sits in the once-per-request walk.

### The two adjacencies

- **Forward (`from:`)** — a row's own `hrLinks`, each `Ref` resolved to a row.
  Resolution is the reverse of `refSpellings` (`Query.hs`:647-648) across three
  namespaces: `hrId`, the title, and `idPropertyOf` for `ViaOrgId`. Note that
  `feRef` is a LINEAR `find` today (`Filter.hs`:421), so the walk builds one
  name→id map per request instead of calling it per edge.
- **Backward (`ref:`)** — target → sources, which is the in-edge index the
  relations proposal already specified (`../partial/2026-08-12-relations.md`:341-356).
  Without it: one pass over `V` per level, testing each row's `hrLinks` against
  the frontier — the shipped `ref:` test with a set on the right.

### Complexity, honestly

- **Worst case is `V + E` per query**, and the cap does not lower it: a store
  where everything reaches everything reaches saturation at small N.
- **The corpus says the worst case is a different corpus.** Over `~/sync`:
  10452 rows, 3524 edges, 82.7% of rows touching no edge and 91.2% pointed at by
  nothing (`../partial/2026-08-12-relations.md`:60-74). The reached set is small
  because the graph is sparse.
- **Without the index**, the backward walk costs at most N times what `ref:`
  costs — measured at **1.42 ms** (`../partial/2026-08-12-relations.md`:343), so
  depth 3 is ≈ 4.3 ms and the cap at 8 is ≈ 11 ms. That fits inside the budget
  the repo already quotes for a filter keystroke to painted rows, 140 ms
  (`docs/plan-org-console-web.md`:535). **The closure ships without the index.**
- **With the index** the same call is 2.39 µs, a 595× at 40 ms of build and
  2.3 MB (`../partial/2026-08-12-relations.md`:343-344). It is a phase-4
  swap of one function — and **the sibling already builds that map**, name → the
  edges naming it, as a per-request fold bound lazily behind `*any*`. So phase 1
  can read the sibling's builder rather than rolling a second one, and phase 4
  is what moves the map onto the store. Nothing above depends on which: the walk
  asks for an adjacency function and does not care where it was built.
- **Cycles are real.** The census makes no acyclicity claim, and a mutual pair
  is one `?kind=` on each side — the peer draws `> kind` / `< kind` for exactly
  that (`../partial/2026-08-12-relations.md`:390-391). The visited set is the
  whole of the answer.
- **Memory** is at most `|V|` ids per closure atom, released with the request;
  the ids are already `T.copy`-detached by the record's own forcing
  (`Query.hs`:1211-1217).

## The DSL surfaces

### F — the typed DSL

The sibling delivers `refs_to` / `refs_from` as single-hop predicates. This
proposal adds ONE kwarg:

```haskell
.filter(refs_to("abc123", depth = 3, kind = "blocked-by"))
.filter(refs_from("abc123", depth = 2))
```

The Haskell-honest spelling is the kwarg, on the DSL's own law — *a kwarg BINDS
a field the stage can carry, a positional arg is the thing the stage is ABOUT*
([the typed DSL](2026-08-21-the-typed-dsl-behind-the-dot-door.md):55-57). The
id is what the predicate is about; the depth and the kind bind.

Two spellings were weighed and lose:

- `refs_to("ID", depth <= 3)` — a comparison in argument position. The DSL admits
  comparison operators on the TEMPORAL fields and nowhere else
  ([the typed DSL](2026-08-21-the-typed-dsl-behind-the-dot-door.md):519-527), so
  this either widens that domain for one argument or makes `depth` a special
  form. `depth = 3` says the same and stays inside the roster law.
- `refs_to+("ID")` — an operator in the name, which cannot carry the bound, for
  the same reason `ref+:` cannot.

The bare `refs_to("ID")` stays depth 1, so law 1 holds on this surface too.

### g — and the `WITH RECURSIVE` refusal

A bounded closure is what SQL calls `WITH RECURSIVE`, and **variant g refuses
it, with a named diagnostic.** The ground is the variant's own central law: g
refuses a cross-axis `OR` because it has no flat spelling, and a filter that
quietly drops a conjunct serves MORE rows
(`spikes/2026-08-21-dot-chain-box/g-sql.html`:20-27). A recursive CTE is a
NAMED SUBQUERY with an arbitrary body, of which the flat grammar composes
exactly one shape; accepting the keyword and refusing every body but one would
teach a language that is not there.

So the closure is a predicate in `WHERE`, and the keyword gets a refusal that
names the spelling that works:

```
WITH RECURSIVE …          no recursive CTE: the closure is a predicate —
                          WHERE refs_to('abc123', 3)
```

This is the join refusal's sibling and lands in the same hint row, in the same
ink.

### The IR's closure leaf

A FOURTH leaf beside `atom`, `meta` and `cmp`
([the typed DSL](2026-08-21-the-typed-dsl-behind-the-dot-door.md):679-700):

```ebnf
closure = "(" "closure" key string int kind ")" ;
kind    = "any" | "(" "kind" string ")" ;
```

```
ref:abc..3                    →  (closure ref "abc" 3 any)
ref:abc..3?kind=blocked-by    →  (closure ref "abc" 3 (kind "blocked-by"))
from:abc..2                   →  (closure from "abc" 2 any)
```

**One normalization is owed:** depth 1 with no kind prints `(atom ref "abc")`,
so `ref:abc` and `ref:abc..1` print the same bytes — law 1 is an IDENTITY
(`S^(1..1) = S`), and the IR's job is to quotient meaning-equal spellings
together (`:702-706`). This is a different case from the `cmp` leaf's deliberate
refusal to collapse (`:707-713`): there the equivalence turns one term into two
and the operators do not pair under `not`; here it is one term to one term with
a numeral that has a single legal value. A reviewer who wants the leaf kept
distinct can have that instead, at the price of a corpus row asserting two
spellings that serve identical rows print differently.

### The corpus rows this owes

The corpus IS the conformance suite, and a change to the language is a change to
the corpus first (`:986-990`). The pairs:

| DSL | flat | pins |
|---|---|---|
| `refs_to("abc", depth = 3)` | `ref:abc..3` | the leaf |
| `refs_to("abc", depth = 3, kind = "blocked-by")` | `ref:abc..3?kind=blocked-by` | both cuts, in order |
| `refs_from("abc", depth = 2)` | `from:abc..2` | the direction |
| `refs_to("abc", depth = 1)` | `ref:abc` | the normalization — same bytes |
| `refs_to("abc", depth = 0)` | `ref:abc..0` | serves nothing |
| `refs_to("abc", depth = 99)` | `ref:abc..99` | the cap: serves nothing, warns on the surface |
| `WITH RECURSIVE …` | — | g's refusal and its diagnostic |

## Edge cases

| token | reads as |
|---|---|
| `ref:ID..3` | the closure, depth 3, any kind |
| `ref:ID..1` | exactly `ref:ID` — law 1 |
| `ref:ID..0` | serves nothing — the anchor is not its own reference |
| `ref:ID..` | anchor `ID..`, which no row claims → serves nothing |
| `ref:ID..3x` | anchor `ID..3x` → serves nothing; the tail is no decimal run |
| `ref:ID..99` | above the cap → serves nothing; the typed surface warns |
| `ref:ID..3..2` | the RIGHT-MOST mark cuts: anchor `ID..3`, depth 2 — and `ID..3` claims no row, so it serves nothing |
| `ref:A..2\|B..3` | two closure atoms on the ref axis, ORed |
| `+ref:A..2` | joins the ref axis as an alternative |
| `-ref:A..2` | the rows the closure does not serve, anchor included |
| `ref:"ID..3"` | identical to `ref:ID..3` — inner quotes strip |
| `"ref:ID..3"` | free text — the token OPENS with a quote |
| `REF:ID..3` | free text — keys are lowercase |
| `ref:ID..3?kind=Blocked By` | the scanner cuts on the space; quote it whole — `ref:"ID..3?kind=Blocked By"` slugs to `blocked-by` |
| `ref:ID?kind=k..3` | anchor `ID`, kind slug `k..3`, which no edge carries → serves nothing. The depth rides the ANCHOR half |
| `ref:ID..3&kind=x` | `&` is a token separator (`Filter.hs`:145-146) → two tokens, the second free text |

## Implementation sketch

- **`Query.hs`** — the pure walk beside the reference machinery it reads
  (`:572-705`): `closureIn :: Text -> (Text, Int, Maybe Text)` for the two cuts,
  `reachable` above, and the two adjacency builders over `[HeadlineRecord]`. It
  lives here rather than in `Filter.hs` because `GET /graph` will want the same
  walk (`../partial/2026-08-12-relations.md`:358-368).
- **`Filter.hs`:399-425** — `FilterEnv` gains the two adjacencies it needs,
  built once per request beside `feRef`; `emptyEnv` gives `const []`, so a
  locally-filtered path matches nothing exactly as it does for `ref:` today
  (`AGENTS.hs`:2911-2915).
- **`Filter.hs`:597-624** — `keyTest … Ref` cuts the value ahead of `feRef`.
  Depth 1 with no kind takes today's branch byte for byte, which is
  conservativity made structural.
- **`Filter.hs`:561-571** — untouched. `ref` is not a `stamped` key, so
  `atomsUnder` splits on `|` and each atom carries its own depth; `valueFor Ref`
  stays unfolded, and the kind half folds through `kindSlug` inside the ref
  reader.
- **`Store.hs`:68-80** — where the reverse index lands when phase 4 comes:
  `stInEdges :: !(Map Text [Text])`, folded at load and maintained per file in
  `putFile` / `removeFile` (`:209-220`) exactly as `stTags` is by `stepIndex`
  (`:221-228`).
- **The renderer** adds no divergence row: `table-view.js` has no branch for
  `ref:` and reads it as free text already, which is NARROWER — the safe
  direction the plan documents (`docs/plan-org-console-web.md`:1374-1376).
- **`AGENTS.hs`** — `refTest` (`:2589-2603`) gains its bounded twin, and the
  `Note` roster gains four: termination, monotonicity, the identity hop past one
  hop, and the cap answering with no rows rather than a 400.
- **Tests** — `TestFilter.hs` over the drawn graph as a fixture, cycle included,
  one case per worked row and per edge row; `TestServe.hs` one keyed drive over
  the wire; `TestSpec.hs` the model pin.
- **Docs** — `docs/query.md`'s `ref:` bullet gains the depth sentence and the
  one-sentence note about `..`'s second reading; the README crib gains a row.

## Phases

The house pattern: the flat half and the server first, the surfaces after.

| phase | what lands | gate |
|---|---|---|
| **1** | `ref:ID..N`, `from:ID..N`, bare kinds, the cap, the laws, tests | the sibling's `from:` |
| **2** | `?kind=SLUG` along the chain | the sibling's kind test |
| **3** | `depth` kwarg on `refs_to`/`refs_from`, the IR leaf and its normalization, the corpus pairs, g's refusal | the typed DSL landing |
| **4** | `stInEdges`; `feIn` becomes a lookup | relations stage 4 — the sibling builds the same map per request, so this phase is where it moves onto the store |

LOC, honestly — estimated by counting the shapes above against comparable
landings, and phases 3 and 4 are the softer numbers because both depend on code
that is not written:

| phase | product | model + tests + docs |
|---|---|---|
| 1 | ~90 (`Query.hs` ~60, `Filter.hs` ~30) | ~215 (`AGENTS.hs` ~45, tests ~160, docs ~12) |
| 2 | ~25 | ~40 |
| 3 | ~80 | ~60 plus the corpus block |
| 4 | ~40 | ~20 |

Phases 1 and 2 together are ~370 lines, more than half of it model and test.

## Alternatives considered

Recorded as SETTLED, the user's own ruling.

- **General SQL joins and subqueries — refused.** The flat-URL law: joins,
  grouping and nesting are kept out on purpose, which is what keeps a query a
  flat, human-typable URL string
  ([additive-filters](../done/2026-08-20-additive-filters.md):184-187). The
  closure is admitted because it composes one relation with itself a bounded
  number of times and yields a row set on one axis; a join would put a second
  row set's columns in reach, and nothing here does. This is the cross-axis-`OR`
  refusal's sibling, and variant g refuses it in the same place with a named
  diagnostic.
- **Unbounded closure (`ref:ID..*`) — refused**, the flat-URL law's own
  descendant. A URL may not ask for work whose size the reader cannot see, and
  by law 7 an uncapped depth is the same request under another spelling.
- **Result-set expansion (`.expand(refs)`) — refused HERE.** It is a fourth
  constructor in the composition algebra beside filter, sort and columns, and it
  breaks two settled things: the composition table (expansion does not commute
  with filtering — `filter p ∘ expand ≠ expand ∘ filter p`) and the normal form,
  whose one-badge-per-kind fold rests on each stage kind being a monoid
  ([the typed DSL](2026-08-21-the-typed-dsl-behind-the-dot-door.md):640-651).
  It is outside this proposal's law and is named here as its own future
  discussion, if ever.
- **Path queries (`path:A..B`) — out of scope**, and left open. A path is an
  ordered list of rows, where every token in this language is a row predicate
  ([`../partial/2026-08-12-relations.md`](../partial/2026-08-12-relations.md):366-368).
  It wants a surface of its own.

Inert until reviewed.
