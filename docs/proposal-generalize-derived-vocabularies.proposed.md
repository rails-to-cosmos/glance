# Proposal — a sentence that lists a list should read it

**Status:** proposed · **Date:** 2026-08-04

Four places in the daemon spell a vocabulary twice: once as the code that
decides, and once as English that tells a reader what was decided.  One of the
four has already drifted, and the drift is user-visible — `+1y` parses as a
repeater and is refused by `set-planning`, and the refusal sentence does not
mention `y` either way, so nobody can tell whether it is unsupported or
mistyped.

`Glance.Web.Sort` shows the fix in place: `Sort.hs:136-137` builds its refusal
out of `spelled (map fst (drop 1 directions))`, so a direction word added to the
parse table is offered by the refusal the same commit.  These four are the sites
that did not get that treatment.

## The four

**1. Relative dates — already drifted.**  `Glance.Query.planningTimestamp`'s
vocabulary is a `case` at `Query.hs:2556-2567`: `today`, `tomorrow`, `+Nd`,
`+Nw`, `+Nm`.  The refusal at `:2549-2550` is a hand-written English list:

```
…spell it 2026-08-05, 2026-08-05 09:30, +3d, +2w, +1m, today, tomorrow,
or org's own <2026-08-05 Wed>
```

`Data.Org.Types.TimestampUnit` (`Types.hs:544`) carries `Years`, and `shifted`
(`Query.hs:2563-2567`) has no `"y"` arm.  So the parser reads `+1y` in a repeater,
`set-planning` refuses it, and the sentence naming what IS accepted was written
by hand and is the only thing a client has to go on.

**2. `writeHint`.**  `Routes.hs:211` is a hand-written sentence naming
`/headline` and `/command` as the write routes.  The route table one screen up
(`:163-183`) already knows which entries carry `methodPost` — `/config` is a
third, and the sentence does not say so.  The 404 body assertion
(`test/TestServe.hs:10064`) checks only that `/headlines` is mentioned.

**3. `statsHeaders`.**  `Routes.hs:1136-1144` is a hand-written list of six
`X-Glance-*` headers.  `Glance.Query.LoadFailure` (`Query.hs:248-251`) has three
constructors, and `summarise` (`:365-370`) counts them in a `case` with no
wildcard — so a fourth failure kind is named by the compiler at the counting
site and silently counted into no header at the reporting site.

**4. Log scopes and severities.**  Both vocabularies are free string arguments to
`append(scope, sev, message)` (`Glue.hs:115-138`).  The roster exists as a
comment (`:94-97`) and as a hardcoded copy in the suite
(`test/TestServe.hs:4464-4468`).  The sweep at `:4453` is quantified over the
lines three scripted acts happen to emit, with an anti-vacuity guard at `:4459` —
so it is an honest check of a closed world, and a seventh scope that no test act
provokes ships unexamined.  A fourth severity with no CSS rule
(`Style.hs:205-206`) renders in default ink, indistinguishable from `info`.

## The rule they break

`docs/design-rhymes.md:86-87`: "When prose says 'kept in sync', the design owes a
derivation."  Each of these four is prose restating a list that is sitting in the
same module.

## Proposed change

**Relative dates** — lift the vocabulary into a list the `case` and the sentence
both read.

```haskell
-- | The relative forms a planning date may be spelled in, and what each does to
-- today.  ONE list: 'planningTimestamp' dispatches on it and the refusal names
-- it, so a form added here is offered by the refusal the same commit.
relativeForms :: [(Text, Day -> Day)]
relativeForms =
  [ ("today",    id)
  , ("tomorrow", addDays 1)
  ]

-- | The @+N<unit>@ shifts, by org's own unit letters.
relativeUnits :: [(Char, Integer -> Day -> Day)]
relativeUnits =
  [ ('d', addDays), ('w', addDays . (* 7)), ('m', addGregorianMonthsClip) ]
```

The refusal becomes `T.intercalate ", "` over both, exactly the way
`Sort.hs:137` does it.  Whether `('y', addGregorianYearsClip)` joins
`relativeUnits` is then a one-line decision made in the open, rather than an
absence nobody can see.

**`writeHint`** — derive it from the route table:

```haskell
writeHint :: [Route] -> Text
writeHint rs = "writes go to " <> spelled
  [ "/" <> T.intercalate "/" (rtPath r) | r <- rs, methodPost `elem` map fst (rtMethods r) ]
```

This needs the route table lifted out of `httpApp`'s `where` into a top-level
`routeTable`, which pays for itself twice over: the 503 group
(`test/TestServe.hs:6313-6410`, covering 6 of 8 store-needing routes — `/command`
and `/links` have none) and the 405 group (7 cases, none for `/` or `/ws`) both
become `mapM_` over `filter rtNeedsStore routeTable` instead of hand-written
cases that are already two routes behind.

**`statsHeaders`** — pair each failure with its header once:

```haskell
-- | Each load failure and the header that reports it.  The counting 'case' and
-- this list are the same three facts, so a fourth kind cannot be counted into
-- no header.
failureHeaders :: [(LoadFailure, HeaderName)]
```

**Log vocabularies** — two `[Text]` constants in `Glance.Web.Base` beside
`logLinesDefault` (`Base.hs:85`), spliced into the glue as a frozen roster and
read by `Glance.Web.Page.Style` to emit one colour rule per severity.  The
suite's hardcoded list at `TestServe.hs:4464-4468` then asserts against the
export, which turns the closed-world sweep into an open-world one.

## LOC

Added ~28 across the four.  Removed ~14 (four hand-written sentences and the
suite's two hardcoded rosters).  The route-table lift is roughly neutral in
itself and removes ~40 lines of hand-written 503/405 cases while raising coverage
from 6-of-8 and 7-of-9 to complete.

## Risk

The four are independent and can land separately, easiest first.

- Relative dates: the refusal STRING changes, so any test asserting it verbatim
  moves.  Behaviour is unchanged unless `y` is deliberately added, which should
  be its own commit with its own test.
- `writeHint` and the route-table lift: the 404 and 405 bodies change wording,
  which `test/TestServe.hs` pins; no route, method or status moves.
- `statsHeaders`: header names and values must stay byte-identical, since a
  client reading counts off headers is entitled to them
  (`docs/invariants.md`, the `X-Glance-*` rule).
- Log vocabularies: touches the page, so the byte-identity of the served shell is
  the oracle.

None of the four changes a wire field, a row id, a span rule or an on-disk byte.

## Existing precedent

`Glance.Web.Sort:136-137` — the refusal built from the parse table.
`Glance.Query.linkTypes` (`:761-775`), spelled once and read three ways, with the
reason stated: "adding one is one edit rather than three that no test ties
together."  `Data.Org.Config.settingOf`/`settingEdits` (`Config.hs:213-237`),
where the reader and the writer share one key constant so the pragma cannot be
read under one name and written under another.
