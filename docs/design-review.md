# Glance — Design Review

## Strengths

- **Stateful parsing is the right call.** Org-mode's `#+TODO:` pragmas change how subsequent text is parsed, so threading `Context` through the parser is essential, not over-engineering.
- **The `Parse` typeclass + megaparsec** makes grammar extension clean — add a new type, write a `Parse` instance, wire it into the `Element` choice.
- **IAS with last-writer-wins** is a pragmatic merge strategy for incremental parsing of the same org file over time.

## Issues identified and resolutions

### (1) Existential `Element` replaced with closed sum type

The existential `Element` gave heterogeneous lists but at a steep cost: no pattern matching without `Typeable` casts, no generic traversals, runtime type-checking in every consumer. Since the set of top-level element kinds is small and known, a closed sum type (`EHeadline | EPragma | ETimestamp | EToken`) gives compile-time exhaustiveness checking and simpler code.

### (2) Monolith split into modules

`Data.Org` (815 lines) split into:
- `Data.Org.Types` — all data types, typeclasses, non-parsing instances
- `Data.Org.Parser` — `Parse` typeclass, all `Parse` instances, `orgParse`
- `Data.Org` — re-export facade

Stale parallel hierarchy (`Data.Org.Base`, `Data.Org.Context`, `Data.Org.Timestamp`, `Data.OrgElement`) removed.

### (3) `Headline` Semigroup replaced with named function

`h1 <> h2` silently picked the headline with the later schedule, discarding the other — surprising for `Semigroup` which readers expect to combine data. Replaced with an explicit `resolveHeadline :: Headline -> Headline -> Headline`.

### (4) `Headline` Monoid removed

`mempty` had `Indent 1` (not a true identity element) and the monoid laws didn't hold under the schedule-based `<>`. Replaced with a plain `defaultHeadline` value.

### (5) DB as search index — decision recorded

`PersistentHeadline` is intentionally a flat projection (search index), not a full mirror of the AST. Schema divergence from `Headline` is expected and acceptable.

### (6) Token as catch-all — intentional

Opaque `Token` chunks enable incremental development: text that will later be parsed as paragraphs, links, or markup is captured today rather than rejected. New element types can be added to the `Parse Element` choice list as they're implemented.

### (7) Error recovery — proposal

Currently `orgParse` stops at the first parse error. For large org files, partial results are essential.

**Proposed approach:** use megaparsec's `withRecovery` to:
1. Record each parse error
2. Skip to the next line boundary (or next headline marker `*`)
3. Continue parsing the rest of the input

This changes the signature from returning `Maybe ParseErrorBundle` to returning `[ParseError]`, giving callers a list of all problems encountered alongside all successfully parsed elements.
