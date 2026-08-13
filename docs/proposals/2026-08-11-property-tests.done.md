# Proposal — the parser's laws are universals, so test them as universals

**Status:** done — DELIVERED 2026-08-11 · see [What shipped](#what-shipped) at the foot
· **Date:** 2026-08-11 · **Origin:** user, after a session
where four bugs shipped past 1781 green tests and were caught by looking — an
edit overlay that covered the document under it (fixed in `cb6db85`), a flag
drawn in the warning orange, an empty paragraph collapsing to zero height, badge
colours lost in an Elm port. Those four are the renderer's, and nothing here
reaches them. What the session exposed is the shape: this repo states its rules
as UNIVERSALS and pins them with a hand-chosen example each. The span layer is
where that gap is cheapest to close, because the universals are already written
down.

## The measurement that decides it

Eight laws, each written as a universal in `CLAUDE.md` and again in
`docs/invariants.md`, each pinned by a hand-written example list:

| Law, as written | Where | Examples pinning it |
| --- | --- | --- |
| `hsFull` is a left fold over `spanParts`, ending at the LAST present part | `CLAUDE.md` Spans; `invariants.md:36` | 24 documents (`TestSpans.hs:53`) |
| Sub-spans nest inside `hsFull`, ordered, non-overlapping | `CLAUDE.md` Spans; `invariants.md:46` | the same 24, through `assertInvariants` (`TestSpans.hs:167`) |
| Every span reparses to what it came from | `CLAUDE.md` Spans; `invariants.md:20` | the same 24, twice (`TestSpans.hs:223`, `:227`) |
| `stripSpans` covers every span-carrying constructor | `CLAUDE.md` Spans; `invariants.md:58` | the compiler, plus nothing semantic |
| Subtree extents tile; consecutive ones meet exactly | `CLAUDE.md` Spans; `invariants.md:73` | 5 fixtures (`TestSubtree.hs:151`) + a 64-file corpus sample |
| Decompose → recompose is byte-identical | `CLAUDE.md` Architecture | 12 fixtures under ONE `testCase` (`TestQuery.hs:512`) |
| `applyEdits` accepts exactly the disjoint sets | `CLAUDE.md` Commands; `Edit.hs:68` | 10 splice cases + a validation group (`TestEdit.hs:159`) |
| A timestamp survives render → parse | `CLAUDE.md` Parser; `TestRoundtrip.hs:25` | 19 parse cases + 26 roundtrip rows |

That is ~100 examples over eight universals, and the ninth column nobody has:
a generator. `glance.cabal:306` lists the suite's dependencies — `tasty`,
`tasty-hunit`, and no property library of any kind. `grep` over the repo finds
zero occurrences of `QuickCheck`, `hedgehog`, `smallcheck` or `falsify`.

The corpus reads FOUR of the eight, and only in one direction — see
[What the corpus already does](#what-the-corpus-already-does-and-what-it-does-not).

## The library

Measured on this machine today:

- `~/.local/state/cabal/store/ghc-9.6.7/` already holds **QuickCheck 2.15.0.1,
  2.16.0.0, 2.17.1.0 and 2.18.0.0 BUILT for this compiler**, with
  `random-1.2.1.3`/`1.3.1`, `splitmix-0.1.3.2`, `tasty-1.5.3`/`1.5.4` and
  `tasty-hunit-0.10.2` beside them.
- `~/.cache/cabal/packages/hackage.haskell.org/` carries the QuickCheck, random
  and splitmix tarballs.
- `hedgehog`, `tasty-hedgehog`, `tasty-quickcheck` and `falsify` are in NEITHER
  the store NOR the tarball cache.

So QuickCheck costs zero fetches and hedgehog costs a download. The repo's
standing rule about fetching is `make elm-test`, held OUT of `cabal test`
because elm-test fetches `elm-explorations/test` at run time (Makefile) — a
check that needs the network lives behind its own target. A test dependency the
build cannot resolve offline lands on the wrong side of that line.

**Decision: QuickCheck, and NO `tasty-quickcheck`.** The integration is ~25
lines in `TestDefaults`:

```haskell
-- | Run PROP as one HUnit case.  The seed is FIXED so a run is reproducible;
-- GLANCE_QC_SEED unfixes it, the way GLANCE_CORPUS names a root.
testProperty :: QC.Testable p => String -> p -> TestTree
testProperty name p = testCase name $ do
  seed <- maybe defaultSeed read <$> lookupEnv "GLANCE_QC_SEED"
  r <- QC.quickCheckWithResult (args seed) p
  case r of
    QC.Success{}   -> pure ()
    QC.Failure{..} -> assertFailure (unlines (reason : failingTestCase)
                                     <> "\nreplay: GLANCE_QC_SEED=" <> show usedSeed)
    other          -> assertFailure (QC.output other)
```

`tasty-quickcheck`'s whole buy is three CLI flags (`--quickcheck-replay`,
`--quickcheck-tests`, `--quickcheck-max-size`) and one report line. The shim
buys the same replay through an environment variable the repo already spells
that way (`GLANCE_CORPUS`, `TestDefaults.hs:277`), and it is the one package
here that would otherwise have to be fetched.

The case FOR hedgehog is integrated shrinking — no `Arbitrary` instances, no
hand-written `shrink`. It loses on two counts: it is not on the machine, and the
generator below is one hand-written record with hand-written shrinkers under
either library, which is where integrated shrinking buys least.

## Files

New: `test/TestGen.hs` (the generator and its renderer), `test/TestProperties.hs`
(the properties). Edited: `test/Spec.hs` (one import, one entry in the list at
`:23`), `test/TestDefaults.hs` (the shim above), `glance.cabal` (two
`other-modules`, one `build-depends`).

The layout is the suite's own: one module per subject, each exporting `spec ::
TestTree`, all named in `Spec.hs`'s single list. `-Wmissing-export-lists` is on
(`glance.cabal:305`), so both new modules carry one.

## The laws, and the property beside each

Three kinds, and the kind decides which alphabet the generator draws from
(below).

**ANSWER properties** — the generator computed the offsets, so the assertion is
an equality against a value the parser never saw.

> `hsFull` is derived, never stored: a left fold of `<>` over `spanParts` seeded
> with `hsStars`. Starts at the stars, ends at the LAST present part in source
> order — never a maximum over ends. (`CLAUDE.md`, Spans)

```haskell
prop_spansAreWhereTheyWereWritten spec =
  let (doc, want) = render spec
      (elems, _ctx, err) = orgParse defaultContext doc
  in err === Nothing .&&. map (spanParts . spans) (headlinesOf elems) === map exParts want
```

The generator emits each component and records the span it emitted it at, so
this compares the parser's eight sub-spans against offsets counted by a
different piece of code. Every span-slice assertion in `TestSpans` today is
self-consistent — the slice is checked against the component the SAME parse
produced (`headlineSpanParts`, `Types.hs:275`). This is the first assertion in
the repo that is not.

> a part appended to `spanParts` out of source order silently shortens every
> extent past it (`invariants.md:44`)

```haskell
prop_foldEqualsMax hs = spanEnd (hsFull hs) === maximum (spanEnd <$> present hs)
```

The fold and the maximum agree exactly when `spanParts` (`Types.hs:260`) is in
source order, and diverge the moment it is not. This states the failure mode
`invariants.md` describes and today's single fixture — "planning keywords out
of order", `TestSpans.hs:41` — samples once.

> A subtree span runs from a headline's stars to the next headline at its level
> or shallower, else to EOF … surviving extents tile and consecutive ones meet
> exactly. (`CLAUDE.md`, Spans)

```haskell
prop_subtreesTile spec =
  let (doc, want) = render spec
      tops = [ sp | (lvl, sp) <- levelled (subtreeSpans (T.length doc) (heads doc)), lvl == 1 ]
  in  map (sliceSpan doc) tops === map (sliceSpan doc . exSubtree) want
 .&&. T.concat (map (sliceSpan doc) tops) === T.drop (spanStart (head tops)) doc
```

The second conjunct is tiling as a TEXT equation: every byte from the first
star to EOF in exactly one extent. `subtreeSpans` (`Query.hs:1718`) is a
right-to-left stack fold over LEVEL SEQUENCES, and five fixtures
(`TestSubtree.hs:151`) sample that space thinly — `[1,3,2,1]`, a level skipped
on the way down, is the case nobody wrote.

**ALGEBRA properties** — self-consistency and round-trips, which need no answer,
so they take the adversarial alphabet too.

> Sub-spans nest inside it, ordered todo < priority < title < tags < planning <
> properties, non-overlapping; a drawer ends exactly at `hsFull`'s end.
> (`CLAUDE.md`, Spans)

```haskell
prop_nested doc h = conjoin
  [ inside (hsFull (spans h)) sp          | (_l, sp) <- presentSpans h ]
  <> [ spanEnd a <=? spanStart b          | (a, b) <- pairs (presentSpans h) ]
  <> [ spanEnd props === spanEnd full     | Just props <- [hsProperties (spans h)] ]
```

This is `assertInvariants` (`TestSpans.hs:167`) with its 24 documents replaced
by a generator, and it is the property with the LARGEST corpus overlap — see
below.

> Never covers trailing whitespace. (`CLAUDE.md`, Spans)

```haskell
prop_noTrailingSpace doc h = T.stripEnd slice === slice
  where slice = sliceSpan doc (hsFull (spans h))
```

> Element spans are only well-formed + reparseable. (`CLAUDE.md`, Spans)

```haskell
prop_reparses doc ctx e = bareParse ctx (sliceSpan doc (spanOf e)) === [stripSpans (valueOf e)]
```

Threaded with the parse's OWN final context, which is what a generated
`#+TODO:` line makes load-bearing: a slice carrying a custom keyword reparsed
under `defaultContext` reads that keyword as title text. Today that hazard is
one fixture (`customTodoSpan`, `TestSpans.hs:354`). The corpus never reparses a
span at all.

> `stripSpans` must cover every span-carrying constructor. (`CLAUDE.md`, Spans)

Totality is the compiler's (`-Werror=incomplete-patterns`). The SEMANTIC half —
that a stripped element carries no offset information — is unguarded, and is
what makes ~150 assertions span-blind:

```haskell
prop_offsetInvariant spec pad =
  bare (parse (render spec)) === drop (elementsIn pad) (bare (parse (pad <> render spec)))
```

Move the document by a preamble and the stripped elements must not move. A
constructor that leaks a span fails this and nothing else.

> Decompose → recompose is byte-identical. (`CLAUDE.md`, Architecture)

```haskell
prop_lensIdentity r  = recomposedSubtree r (headlineParts r) === subtreeText r
prop_lensIdempotent r b' = headlineParts (reparse (recomposedSubtree r parts')) === parts'
  where parts' = (headlineParts r) { hpBody = b' }
```

The four-region lens (`Query.hs:1306`, `:1344`) cuts the planning line, the
headline's own drawer and its own logbook out of a subtree and puts them back at
line indices computed by subtraction (`bodyLine`/`taken`, `Query.hs:1358`).
Region PRESENCE is three independent bits, region STYLE is indentation ×
line-ending × ordering, and the arithmetic's failure mode is a region landing
one line late. `TestQuery.hs:512` runs the identity over 12 fixtures inside one
`testCase`; the second property above is the one that exercises the
subtraction, since it re-decomposes a body the client changed.

The byte-ownership half, stated once:

```haskell
prop_oneOwnerPerByte r = sortOn spanStart (regionSpans r) `tiles` subtreeText r
```

> `applyEdits` … Any order, pairwise non-overlapping; the sort is stable, so two
> INSERTIONS at one offset land in LIST order. (`Edit.hs:66`)

```haskell
prop_acceptsExactlyDisjoint doc es = isRight (applyEdits doc es) === legal doc es
prop_permutationInvariant doc es = distinctStarts es ==>
  applyEdits doc es === applyEdits doc (permute es)
prop_lengthAlgebra doc es = legal doc es ==>
  T.length <$> applyEdits doc es === Right (T.length doc + sum (delta <$> es))
prop_identityEdit doc sp = inBounds doc sp ==>
  applyEdits doc [Edit sp (sliceSpan doc sp)] === Right doc
```

`legal` is written in the test as the QUADRATIC pairwise check plus the bounds
test — the specification. `applyEdits` sorts and checks NEIGHBOURS
(`Edit.hs:108`), and that reduction from quadratic to linear is exactly what a
property should be asked to justify. `prop_identityEdit` is "untouched bytes
stay byte-identical" (`CLAUDE.md`, Architecture) as a universal; `TestEdit`'s
10 splice cases (`TestEdit.hs:186`) are four points of that algebra.

> Timestamp range halves share one bracket kind; `tsmHasTime` alone decides
> whether a time renders; the weekday is recomputed from the date.
> (`CLAUDE.md`, Parser)

```haskell
prop_timestampValueRoundtrip ts = parseTimestamp (showt ts) === Just ts
prop_renderIsCanonical ts = showt <$> parseTimestamp (showt ts) === Just (showt ts)
prop_weekdayIsDropped ts wd = parseTimestamp (withWeekday wd (showt ts)) === Just ts
```

The value round-trip is the direction that is TOTAL: render → parse → equal.
Text → render is lossy by design, because the weekday is recomputed, which is
why `TestRoundtrip`'s exact-vs-stable column exists (`TestRoundtrip.hs:25`) and
why its 26 rows all spell canonical stamps. `prop_weekdayIsDropped` is the
"letters in ANY script, any length" rule (`CLAUDE.md`, Parser) as a universal;
today it is one Dutch fixture (`TestSpans.hs:71`). And this is the property that
reaches `compactly`'s three guards (`Types.hs:491`), of which `invariants.md`
records **only the flag is exercised**: a generated `tsCompactRange = True` over
two different days, or over an untimed end, must take the `--` arm and come
back equal.

**NEGATIVE properties** — the documented refusals, over the adversarial list.

> A top-level element must end at whitespace or EOF; a sub-parser stopping
> mid-word fails the WHOLE file (`CLAUDE.md`, Parser)
>
> `orgParse` on error returns zero elements AND the caller's context untouched.

```haskell
prop_failureIsTotal spec = badly spec ==> case orgParse ctx (render spec) of
  (elems, ctx', Just _err) -> elems === [] .&&. ctx' === ctx
  (_, _, Nothing)          -> property Discard
```

`TestNegative` (220 lines) is where these live today, and it is where the
adversarial list belongs.

## The generator, which is the whole of the work

### Three designs, and why one

**1. Structure-first: generate a spec, render it, and keep the offsets.**
CHOSEN. The generator builds an abstract document, spells it out, and RECORDS
where each component landed. That turns the ANSWER properties above from
self-consistency into equality against an independently computed value — the
only design of the three that can do it.

**2. Mutate corpus documents.** KEPT, as a second and smaller thing behind
`GLANCE_CORPUS`. Real shapes, no generator to write, and a text mutation that
preserves org's grammar is nearly free (insert a tag, permute the planning
entries, add horizontal space, swap `\n` for `\r\n`). Its oracle is only
self-consistency, which is what the corpus scan already gives — and it needs the
corpus, so it cannot run offline or in CI.

**3. Grammar-directed text generation.** REJECTED. It yields valid text without
a renderer, and it does NOT know the answer, so it lands back on
self-consistency for the price of a SECOND GRAMMAR — the exact thing the
doctrine refuses in the product (`CLAUDE.md`, on `ref:`'s known limit: "a second
scanner would be a second grammar to keep in step").

Design 1 duplicates something too, and the distinction is the argument for it:
**a renderer is a SPELLING, a parser is a RECOGNITION**. Org's spelling is fixed
and small — stars, a space, a keyword, `[#A]`, words, `:a:b:`, a planning line,
a drawer between `:PROPERTIES:` and `:END:`. Org's recognition is where every
subtlety in `CLAUDE.md`'s Parser section lives. And the suite already blesses an
independent oracle of exactly this kind: `TestDefaults` hides the library's
`headlinesOf` and re-spells it, "ON PURPOSE: the suite's copy is an INDEPENDENT
ORACLE for the span groups that read it, and one derived from the library would
agree with any change to it" (`TestDefaults.hs:66`). `TestFilter`'s hardcoded
six-cell layout list is the same idea (`CLAUDE.md`, parity discipline: "an
INDEPENDENT ORACLE rather than a mirror").

The renderer must NOT be `TextShow`. `TextShow` is the lossy REPL re-serializer
(`CLAUDE.md`, Render); rendering through it would test the parser against its
own inverse and hide every bug the two share.

### The spec

```haskell
-- | A document as a STRUCTURE — what to spell, never where it lands.
data DocSpec = DocSpec
  { dsKeywords :: Maybe ([Text], [Text])  -- ^ the #+TODO: cycle this document declares.
  , dsEol      :: Eol                     -- ^ LF or CRLF, the DOCUMENT's own.
  , dsEntries  :: [EntrySpec]
  }

data EntrySpec = EntrySpec
  { esLevel      :: Int                   -- ^ 1..4.
  , esTodo       :: Maybe Text            -- ^ drawn from dsKeywords, so recognition is reachable.
  , esPriority   :: Maybe Char
  , esTitle      :: [Text]                -- ^ possibly empty: the blank entry is a case.
  , esTags       :: [Text]
  , esPlanning   :: [(PlanKey, TsSpec)]   -- ^ IN THE ORDER THEY ARE WRITTEN — the permutation.
  , esProperties :: [(Text, Text)]
  , esLogbook    :: [Text]
  , esBody       :: [BodyLine]
  , esIndent     :: Int                   -- ^ the drawer's own indentation.
  , esGap        :: Int                   -- ^ blank lines before the next entry.
  }
```

Every optional part is a `Maybe` or a list, so "absent" is a value the shrinker
can reach and the property list already names each absence as a case. `esGap`
exists because trailing blank lines belong to the subtree above
(`CLAUDE.md`, Spans) and that rule has no other way to be generated.

### The renderer that knows the answer

One pass over a `State Int` offset counter. `emit` is the whole mechanism:

```haskell
type M = State (Int, Builder)

-- | Write T and answer the span it occupies.
emit :: Text -> M Span
emit t = do (at, b) <- get
            put (at + T.length t, b <> fromText t)
            pure (Span at (at + T.length t))
```

Counting in CHARACTERS, which is what spans are (`CLAUDE.md`, Spans) — so a
generator that spells `Привет` and a parser that answers 6 rather than 12 agree
by construction, and the byte/char confusion has a second witness beside
`TestSubtree`'s unicode fixture (`invariants.md:13`).

Each component's `emit` result goes into an `Expected` record shaped like
`HeadlineSpans`, and the whitespace BETWEEN components is emitted without being
recorded — which is what makes "sub-spans are tight" and "hsFull never covers
trailing whitespace" checkable rather than assumed.

### Two alphabets, and the property kind picks one

The PLAIN pool is words the generator can predict the parse of: `["a", "task",
"Привет", "проверка", "note"]`, tag names from `[a-z]`, property keys from a
short uppercase list, priorities from `A`–`D` (`[#D]` is legal and unbadged —
`CLAUDE.md`, priority parity).

The ADVERSARIAL pool is words that CHANGE the parse: `TODO` in title position,
`*bold*`, `[[https://x][y]]`, `:a:` inside a title (org's own grammar reads it
back as a tag run — `CLAUDE.md`, `setTitleEdits`), `<2026-01-01 Thu>`, a word
carrying `::`. These reach the ALGEBRA and NEGATIVE properties, which need no
predicted answer, and stay OUT of the ANSWER properties, where the generator
would be asserting a prediction it has no right to.

The split is the honest form of a decision that has to be made anyway. A
generator over arbitrary `Text` fuzzes the decoder; a generator over a pool with
a stated adversarial half aims at the parser's decision points, which is where
the laws are.

### Shrinking, and what it must leave behind

Shrink the SPEC, never the text. A text-level shrinker produces bytes that need
not be a legal org document, and a minimal counterexample that does not parse
teaches nothing. The moves, in order:

1. drop entries (the whole tail, then bisect),
2. drop optional components — planning entries, the drawer, the logbook, tags,
   the keyword, the priority,
3. shorten lists (title words, body lines, properties),
4. lower `esLevel` toward 1 and `esGap`/`esIndent` toward 0,
5. move each alphabet word toward the pool's FIRST member.

Never below a level of 1, never to an empty document, never a character edit.
The failure message prints BOTH the shrunk spec and the rendered text, so a
reader can paste the second into a file and the first into the suite.

### The generator's own oracle

A property that generates nothing interesting passes forever, which is this
repo's stated failure mode in another dress: `groundSweep` "asserts what it
swept first so an empty sweep cannot pass", and `paletteSweep` derives its
oracle off the served page rather than mirroring it (`CLAUDE.md`, UI). The
generator owes the same, and QuickCheck's `cover`/`checkCoverage` is the
mechanism:

```haskell
prop_imageIsWide spec = checkCoverage $ tabulate "shapes" (shapesOf spec) $
  cover 20 (any (not . null . esPlanning)  (dsEntries spec)) "planning"      $
  cover 10 (any permutedPlanning           (dsEntries spec)) "permuted"      $
  cover 20 (any (not . null . esProperties)(dsEntries spec)) "drawer"        $
  cover  5 (any (not . null . esLogbook)   (dsEntries spec)) "logbook"       $
  cover 10 (any (null . esTitle)           (dsEntries spec)) "blank title"   $
  cover 10 (any ((>= 3) . esLevel)         (dsEntries spec)) "deep"          $
  cover 10 (isJust (dsKeywords spec))                        "custom cycle"  $
  cover 10 (dsEol spec == CRLF)                              "crlf"          True
```

This FAILS when the generator's image is short, which is the only thing that
keeps the other twenty properties meaningful. It goes in first.

## What the corpus already does, and what it does not

`glance scan ~/sync` — 12630 headlines, 0 span violations, ~10 s, run per
release — checks four things per headline (`app/Scan.hs:196`): `spanFaults` on
`hsFull` and every present sub-span, containment inside `hsFull`, non-overlap of
consecutive present sub-spans, and `slice-mismatch` against `headlineSpanParts`.
`TestSubtree`'s corpus group re-checks subtree geometry over a 64-file sample
(`TestDefaults.hs:289`), and `TestEdit`'s canary round-trips single spans over
≤50 headlines (`TestEdit.hs:460`).

**Where the overlap is total.** `prop_nested` and the well-formedness half of
the span properties ask the corpus's own questions of ~100 generated headlines
where the corpus asks them of 12630 real ones. Those two properties buy
OFFLINE + SHRINKING, and nothing else. Stated plainly so nobody claims more.

**What properties buy over it.**

- **Reparse.** The corpus never reparses a span. `prop_reparses` and the
  `hsFull` reparse are `TestSpans`'s 24 documents and nothing else — and the
  hazard they cover (a custom `#+TODO:` keyword read as title text under the
  wrong context) is one fixture wide today.
- **The lens.** No corpus canary exists for decompose → recompose. 12 fixtures
  is the whole of it, and region presence alone is 2³.
- **`applyEdits`' algebra.** The corpus canary round-trips ONE span at a time.
  Permutation invariance, the acceptance boundary and the length algebra are
  unreachable from a read-only scan: they are claims about SETS of edits.
- **Timestamps.** No corpus check at all; the three `compactly` guards are
  recorded as unexercised.
- **The fold-vs-max distinction.** The corpus only ever sees spans a CORRECT
  `spanParts` produced, so it cannot witness an out-of-order append. That
  failure mode is written down (`invariants.md:44`) and guarded by one fixture.
- **Level sequences.** The corpus has whatever nesting the author wrote;
  `[1,3,2,1]` and a 4-deep entry followed by a level-1 are generated for free.
- **CI and offline.** The corpus is one person's tree, is not in the repo, and
  is skipped by default with a line on stderr (`TestDefaults.hs:287`). A green
  run without those lines is unverified on the corpus half — which is most runs.

**Where properties are strictly weaker.** The corpus's image is what people
wrote over years: four superseded `ORG_GLANCE_ID` generations, Dutch weekdays,
`::` inside titles, a hyphen in a commented `#+TODO:`, 11 files of 6290 that
fail to parse and are a documented failure CLASS. A generator's image is the
union of what its author thought of, and the coverage assertion above measures
that union against itself. **The corpus stays, per release, unchanged.**

## LOC estimate

| | LOC |
| --- | --- |
| `test/TestGen.hs` — spec types, generators, offset-tracking renderer, shrinkers, coverage | ~400 |
| `test/TestProperties.hs` — ~22 properties in tasty groups | ~280 |
| `test/TestDefaults.hs` — the `testProperty` shim | ~25 |
| `test/Spec.hs`, `glance.cabal` | ~5 |
| **Total, all of it test code** | **~710** |

Product LOC: **zero**. Build-depends: **+1** (`QuickCheck`), already built in the
store for this compiler.

Delivery order, each step green on its own: the shim and
`prop_imageIsWide` first (the generator asserted before anything is read through
it), then the ALGEBRA properties (no `Expected` record needed), then the offset
tracking and the ANSWER properties, then the negative list, then the corpus
mutation group behind `GLANCE_CORPUS`.

## Risk

- **Non-determinism.** A random seed makes `cabal test` a different test each
  run and cannot gate a commit. The shim fixes the seed; `GLANCE_QC_SEED`
  unfixes it. The cost is honest and stated in [Open decisions](#open-decisions).
- **A property that asserts nothing.** The session's own hand-run mutation
  round found two of these (a harness stub whose `markAll` returned `undefined`
  where the documented handle returns a count; a CSS permission nothing
  exercised). `prop_imageIsWide` is the structural answer, and it lands first.
- **Generator bugs read as parser bugs.** Mitigated by the delivery order: the
  ALGEBRA properties need no `Expected` record, so the offset tracking is
  trusted only after the generator has already been rendering documents the
  parser accepts. A failing ANSWER property is checked against the rendered text
  by eye before it is called a parser bug — which the shrinker's org-text output
  is for.
- **Runtime budget.** ~22 properties × 100 cases × one parse of a ≤2 KB
  document. The budget to hold is **under 5 s added to `cabal test`**; measure
  before it lands and cut the case count per property if it does not hold.
- **The renderer is a second spelling to keep in step.** Real, and bounded: org's
  spelling is fixed, and a change to it is a change to the parser that the
  ANSWER properties will fail loudly. This is the cost paid for the independent
  oracle, and the repo pays it twice already (`TestDefaults`' `headlinesOf`,
  `TestFilter`'s layout list).

## Existing precedent

- **Independent oracles in the suite.** `TestDefaults.hs:66` re-spells
  `headlinesOf` on purpose; `TestFilter` keeps a hardcoded six-cell layout list
  as "an INDEPENDENT ORACLE rather than a mirror"; `TestServe.paletteSweep`
  reads the served page and compares the two namespaces role by role.
- **Assert the instrument before reading through it.** `groundSweep` asserts
  what it swept first; `domSpec` reports what the harness's own selectors find
  before any case reads through them; `TestSelfContained` asserts what it swept.
- **A check with a cost lives behind its own switch.** `GLANCE_CORPUS` names a
  root and says `SKIPPED` on stderr when unset; `make elm-test` is out of
  `cabal test` because it fetches.
- **`stripSpans` is the model for the offset-invariance property**: the rule is
  already written as a universal with a stated blast radius (~150 assertions),
  and the compiler covers only its totality half.

## Open decisions

1. **THE ONE A HUMAN OWES: fixed seed inside `cabal test`, or a random seed
   behind `make prop`.** Fixed means the properties are 100 more deterministic
   examples that can gate a commit and will never find anything new on their
   own. Random means they keep finding counterexamples and cannot gate anything,
   because a red run is not reproducible from the commit alone. The repo's
   precedent points both ways: `GLANCE_CORPUS` says the expensive half rides an
   environment variable, and `make elm-test` says a check that cannot run
   offline lives outside the suite — neither is about non-determinism. The
   proposal's default is **fixed seed in `cabal test`, plus `make prop` running
   the same module at a random seed**, which is two lines of Makefile; a human
   should confirm that a nightly nobody runs is worth having.
2. **Whether the corpus-mutation group is worth its weight** once the structured
   generator exists. It shares the corpus's oracle exactly and cannot run in CI.
   Cheap to add later; nothing else depends on it.
3. **Whether the adversarial pool grows into `TestNegative` or stays in
   `TestGen`.** The negative properties are about documents the parser is
   DOCUMENTED to refuse, and that list is `TestNegative`'s subject already.

## What shipped

`test/TestGen.hs` (the spec, the offset-tracking renderer, the two alphabets,
the shrinkers, the census), `test/TestProperties.hs` (24 cases in seven groups),
the `testProperty` shim in `TestDefaults`, one `Spec.hs` entry, and QuickCheck
2.18.0.0 in `glance.cabal` — resolved from the store, zero fetches. Product LOC:
**zero**. Suite: **1789 → 1813**, the group standing alone in **0.39 s**, which
is inside the run-to-run noise of the whole suite and well under the 5 s budget.
Open decision 1 landed as its DEFAULT half — fixed seed inside `cabal test`,
unfixed by `GLANCE_QC_SEED`. The `make prop` half and open decision 2's
corpus-mutation group are NOT delivered.

### Six live findings, each a documented universal the generator falsified

Each is reproduced by the org text beside it and none is fixed here.

1. **A headline whose keyword or priority is the last thing on its line eats the
   next line.** `todoP` and `priorityP` are `lexemeP`s, whose trailing
   `MPC.space` crosses the NEWLINE. `* TODO\n* Next\nmore` is ONE headline,
   titled `* Next`; `* TODO\nSCHEDULED: <2024-01-15 Mon>` reads the planning
   line as the title and carries no schedule; `* [#A]\n:PROPERTIES:\n:ORG_GLANCE_ID:
   x1\n:END:` loses the drawer and the id — the same class as the Dutch weekday,
   which cost 28 blobs their id. `TestNegative` pins `* TODO` at END OF INPUT,
   where nothing follows to be eaten.
2. **Trailing horizontal space on a title line detaches the planning line.**
   `planningP` opens with `MPC.eol` and nothing consumed the spaces, so
   `* Task  \nSCHEDULED: <2024-01-15 Mon>` carries no schedule and the stamp
   becomes a top-level element. Same for `* Task :x:  `.
3. **A blank top entry breaks the tiling.** `* Task\nbody\n* \nmore\n* Last\n`
   yields extents `[0,12)` and `[20,27)`: the blank entry keeps its extent in
   `subtreeSpans` and loses its record in `recordsOf`, so `[12,20)` belongs to no
   row. `CLAUDE.md` says surviving extents tile and consecutive ones meet
   exactly.
4. **Decompose → recompose is not byte-identical for an EMPTY drawer.**
   `* a\n:PROPERTIES:\n:END:\n` comes back `* a\n`. `HeadlineParts` carries no
   bit for a drawer's presence, so a drawer holding no pair reads as a client
   that emptied one. A materialize and an unmodified flush delete it.
5. **…nor for a planning line spelling one keyword twice, or closed by
   horizontal space, or unterminated at EOF.** `CLOSED: <a> CLOSED: <b>` comes
   back as `CLOSED: <b>` (the parse is last-wins, the region is the whole line,
   the recompose writes what survived); `CLOSED: <a>  ` loses the two spaces
   (`planningText` rebuilds from each entry's raw text plus the line ending, and
   what trails the last entry is in neither); a file whose last line is an
   unterminated `CLOSED: <a>` comes back one newline longer.
6. **A `Restart` repeater signed `TRSMinus` is outside the parser's image and
   does not round-trip.** `repeaterFormat` spells it `-4d`, which
   `tsCookieParser` tries as a WARNING first. Unreachable from any org text, so
   this is the `Timestamp` value space being wider than the parse rather than a
   defect — stated because the generator has to know it.

Findings 3, 4 and 5 are excluded from the properties they falsify, by a named
predicate carrying the reason. Findings 1 and 2 are excluded at the generator
(`TestGen.normEntry`, and trailing space emitted on an entry's LAST line only).

### Where the delivery diverges from the plan above

- **`prop_oneOwnerPerByte` is weaker than proposed.** `regionSpans` is
  module-private, so the tiling equation is unreachable; what ships is the body's
  lines being a SUBSEQUENCE of the subtree's, plus the logbook not also being in
  the body.
- **The coverage assertion is a deterministic census, not `checkCoverage`.** One
  400-document sample under a fixed generator seed, with a floor per shape and
  the whole census printed on failure — an exact count rather than a statistical
  one, which is what `groundSweep` does. A second census counts both sides of
  `applyEdits`' acceptance boundary.
- **Two `applyEdits` properties take sets that are disjoint BY CONSTRUCTION.**
  Only a fifth of the adversarial sets are legal, and a precondition discarding
  four cases in five is a property mostly not being run; legality is asserted
  inside each rather than assumed.
- **The lens idempotence property compares the logbook with its terminator
  stripped.** A region that was the file's last line and no longer is must gain
  one, which is the splice being right.
- **`subtreeSpans` is reached through `loadFile`,** not directly: it is not
  exported from `Glance.Query`, so the tiling property reads the records a store
  would hold and therefore also covers `recordsOf`'s two filters.
