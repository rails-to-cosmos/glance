# Proposal — `columns:` takes `+` and `-`, and the set becomes a fold

**Status:** proposed · **Date:** 2026-08-11 · **Origin:** user

## Pattern

`columns:` states the column set WHOLESALE. `columnNamesIn`
(`src-web/Glance/Web/Columns.hs:37`) collects every `columns:` token's names in
written order, dedups first-wins case-folded (`:49-52`), and hands the list to
`Glance.Query.resolveColumns` (`src-query/Glance/Query.hs:3180`), which the
route substitutes for the default view (`src-web/Glance/Web/Routes.hs:326`).
A reader who wants the default six PLUS one property column spells all seven.

The ask: `columns:+NAME` adds to the current set, `columns:-NAME` removes from
it.

The set is already a LEFT FOLD over the query's segments — `foldl extend []`
at `Columns.hs:42` — seeded with the empty list. The whole feature is a
different seed and two more segment kinds. Nothing below the grammar moves:
`resolveColumns`, `viewJSONFor` (`Query.hs:3086`), the wire and the renderer
are handed a list of NAMES either way.

## Proposed change

### 1. THE CURRENT SET IS WHAT THE QUERY HAS COMPOSED, SEEDED WITH THE DEFAULT VIEW

The fold's seed becomes `filterKeys` (`Query.hs:3217` — the default view's own
keys in draw order, `Filter.hs:15` re-exporting them), and **THE FIRST ABSOLUTE
NAME SPENDS THE SEED**: a reader naming a column outright states the whole set,
so what stood — the default included — goes, and every segment after it
composes onto what the query has built.

- `columns:state,title` → `[state, title]`. Today's answer, unchanged.
- `columns:state columns:title` → `[state, title]`. Repeats compose, unchanged
  (`Columns.hs:13-15`).
- `columns:+notes` → the default six with `notes` at the end.
- `columns:-tag` → the default five.
- `columns:state,title columns:+notes` → `[state, title, notes]`.

`resolveColumns filterKeys` is `viewColumns` name for name — every key resolves
to its own builtin (`Query.hs:3186-3188`) — so a query whose modifiers move
nothing serves the default view byte for byte. That identity is what pins the
seed and it is a test case below.

**What breaks under the other reading** (the current set is ALWAYS the default
view, so `+` and `-` are read against `viewColumns` whatever else the query
says): the view-plus-refinement workflow, which is the whole point of `+`.
`view:NAME` is a MACRO the shell expands into that view's own query
(CLAUDE.md), so a saved view holding `columns:state,title,tag` arrives as an
applied query carrying that absolute token; the reader then types
`columns:+notes` onto it. Under the default-view reading the token's base is
the default six and the query names two different sets with no composition rule
that is not invented. Under the fold there is one set and one order.

### 2. MIXING IS ALLOWED, IN ONE TOKEN AND ACROSS TOKENS

`columns:+notes,-tag` is one token with two modifier segments;
`columns:State,Title columns:+notes` is a mix across tokens. Both are read the
one way: segments are a single sequence in written order, tokens and commas
alike, which is `sort:`'s `->` rule read onto the comma (`Sort.hs:107-110`).

First-wins dedup extends without a new rule, because it was always a property
of the ACCUMULATOR rather than of the token list:

- `+x` where `x` already stands is a no-op and `x` keeps its place — first-wins,
  said from the other side.
- `-x` where `x` is absent costs nothing, the rhyme `setPriorityEdits` already
  carries (CLAUDE.md: "Clearing a headline that carries none costs no edit").
- Both comparisons are case-folded, as `same` is today (`Columns.hs:52`), so
  `-Tags` takes the `tag` column out.

An absolute name written AFTER a modifier restates the whole set:
`columns:+notes columns:state` is `[state]`, and the `+notes` is gone. This is
the one place written order discards something a reader wrote, and it is the
open decision below.

### 3. AN ADDED COLUMN LANDS AT THE END, AND `-x +x` DOES NOT RESTORE ITS PLACE

The set is an ordered list built left to right and nothing records a position a
name used to hold. `columns:-state,+state` is the default view with `state`
moved to the far end. Remembering the vacated place would need a second
structure and would make `-x +x` differ from `+x` alone over a set that never
had `x`, which is a rule a reader cannot predict from the query in front of
them.

### 4. THE MINIMAL SET IS TITLE, AND ASKING FOR ITS REMOVAL IS A 400

`columns:-title` (and `-Title`, the header spelling) is refused, naming the
token. The reason is the `order=` lesson (`Routes.hs:410-412`): a request
silently ignored looks like a request that worked, and here it would be worse
than ignored — `resolveColumns`' `withTitle` (`Query.hs:3183-3185`) re-injects
the title FIRST, so a swallowed `-title` would MOVE the column rather than
remove it.

The two layers keep their two jobs: the grammar refuses a reader who asked to
take the title off, the resolver supplies it to a reader who never named it.
They read ONE fact, a new `minimalKey`/`minimalNames` pair beside
`resolveColumns`.

**A query that removes everything else** serves `[title]` and nothing more:
`columns:-state,-priority,-scheduled,-deadline,-tag` leaves one column, and
that is a table a reader can act on. **A query whose fold empties by other
means** — `columns:state,-state` → `[]` — answers `Just []`, which
`resolveColumns` turns into `[title]` by the same wall. This costs one honest
change to today's collapse: `Columns.hs:42-44` maps an empty RESULT to
`Nothing` (the default stands), and the new reading measures emptiness on the
SEGMENTS instead — a query that named nothing keeps `Nothing`, a query that
named something and composed nothing answers `Just []`. Without that split,
"remove everything" would serve everything.

### 5. `->` GAINS NO MEANING HERE

`->` is `sort:`'s sugar because a sort SEGMENT names one column, so a token
naming several needs a separator (`Sort.hs:58-60`). A columns token already
names several, and its separator is the comma. `columns:a->b` is therefore ONE
name spelled `a->b`, which resolves as a custom column reading a drawer key of
that name (`Query.hs:3200`) — the existing total behaviour, and refusing it
would make an arbitrary character unusable as a property key.

### 6. THE RENDERER NEEDS NO CHANGE, AND THIS IS PRODUCER-ONLY IN THE `columns:` SENSE ALREADY

`columns:` was producer-shaped before this proposal: which columns a name
resolves to, and what an unknown name reads out of a row, is the server's
answer arriving as the view's `columns` (`../table-view/web/table-view.js:847-857`).
The renderer's whole involvement is three places and each survives untouched:

- **Free text and the matcher.** `VIEW_KEYS` (`:872`) keeps the token out of
  free text and `queryMatcher` skips it (`:2627`). Neither reads the value, so
  a sign inside it changes nothing.
- **The chip.** `showsColumns` (`:4001-4005`) splits on `,` and asks whether
  any name is nonempty; `+notes` and `-tag` are nonempty, so the token still
  wears `tv-chip-cols` — right, since it still states a column set.
- **The completion.** The `columns` stage's prefix is what follows the last
  comma (`:4278-4282`), so after `+` the prefix is `+notes` and no column key
  starts with `+`: the domain offers nothing and the reader types the name.
  A graceful floor, and no wrong answer — the accept path (`:4678-4682`) is
  never reached with no offer to accept.

So the divergence is `ref:`'s KIND (the producer decides, the renderer defers)
without `ref:`'s cost — `ref:` narrows on one side and not the other, and this
narrows on neither.

**Optional renderer polish, out of this proposal's scope** (~4 lines in
`../table-view`): strip a leading `+`/`-` off the completion prefix at `:4280`
and off the `taken` list at `:4373-4374`, and carry the sign in `head` at
`:4681`, so the domain re-opens after a sign. Worth doing, owed by nothing.

The shell needs nothing either: it composes no `columns:` token anywhere
(`grep columns: frontend/glue/*.js` is empty), and a set arriving different is
already a remount (`frontend/glue/70-shell.js:358`, `00-core.js:82,297`).

### 7. REFUSALS, AND HOW THE TWO MINUSES ARE TOLD APART

**400, naming the token as the reader wrote it** (`refusedOn`,
`Filter.hs:85-90`), coarsest first — the capture path's ordering rule:

1. a negated token, `-columns:state` (`Columns.hs:57`, unchanged);
2. an alternation, `columns:a|b` (`:58`, unchanged);
3. a removal of the minimal column, `columns:-title` / `columns:-Title` /
   `columns:state,-title` (new).

**Silently ignored, each for a reason already written down:**

- `-x` where `x` is not in the set — nothing to take off.
- `+x` where `x` already stands — first-wins dedup.
- an empty segment, sign or no sign: `columns:`, `columns:,`, `columns:+`,
  `columns:-` name nothing, which is the `key:` rule (`Columns.hs:17-19`).

**`-columns:state` and `columns:-state` cannot collide, and the scanner is what
says so.** `scanQuery` takes a leading `-` into `tkNegated` only while nothing
has been seen (`Filter.hs:112`, `not (seen s), c == '-'`), and `splitKey`
(`:168-172`) cuts at the first `:` or `=`. So:

| written | `tmNegated` | `tmKey` | `tmValue` |
|---|---|---|---|
| `-columns:state` | `True` | `Just "columns"` | `"state"` |
| `columns:-state` | `False` | `Just "columns"` | `"-state"` |
| `-columns:-state` | `True` | `Just "columns"` | `"-state"` |

The third is refused as a negation before its value is looked at, which is what
"coarsest first" buys. A sign is read at position 0 or after the colon, never
both, so the two spellings are two terms the parser already tells apart.

**One compatibility consequence, stated plainly:** a name can no longer OPEN
with a sign, so a drawer key spelled `+notes` is unreachable as a bare name.
The escape falls out of "ONE leading sign is read": `columns:++notes` adds the
custom column named `+notes`, and `columns:+-notes` adds `-notes`.

## Files

- `src-web/Glance/Web/Columns.hs` — the whole feature.
- `src-query/Glance/Query.hs` — `minimalKey`/`minimalNames` beside
  `resolveColumns` (`:3176-3198`), two export entries (`:167`).
- `test/TestFilter.hs` — `columnsSpec` (`:560-635`) grows the cases below.
- `test/TestServe.hs` — one wire case.
- `CLAUDE.md` — the `columns:` bullet.
- `CHANGELOG.md` — one `Added` line.
- `../table-view/SCHEMA.md:379-397` — the `columns` paragraph, cross-repo. The
  producer is the stricter side here as it is for `sort:`, which that paragraph
  already records.

Nothing in `src-web/Glance/Web/Routes.hs`, `assets/`, or `../table-view/web/`
changes.

## The Haskell

`src-web/Glance/Web/Columns.hs`, in full below the module header:

```haskell
module Glance.Web.Columns (columnNamesIn) where

import Data.List (foldl')
import Data.Text (Text)

import qualified Data.Text as T

import Glance.Query (minimalNames)
import Glance.Web.Filter ( Term (tmKey, tmNegated, tmValue), columnsKey
                         , filterKeys, parseFilter, refusedOn )

-- | What one segment of a @columns:@ token asks OF THE SET.
data Asked
  = Absolute !Text  -- ^ a bare name: the set stated outright.
  | Add !Text       -- ^ @+NAME@: the name joins the END of the set.
  | Take !Text      -- ^ @-NAME@: every spelling of the name leaves the set.

-- | The column names Q states: 'Nothing' where Q's columns tokens name nothing
-- at all — the default view stands — the composed set where they name
-- anything, and 'Left' naming the token where one cannot be read.
--
-- THE SEED IS THE DEFAULT VIEW AND THE FIRST ABSOLUTE NAME SPENDS IT: a reader
-- naming a column outright states the whole set, so what stood goes and every
-- segment after it composes onto what the query has built.  A query of
-- modifiers alone therefore answers the default view moved, and one whose
-- modifiers move nothing answers it unchanged.
--
-- The emptiness that falls back to the default is the SEGMENTS', never the
-- result's: a query that named something and composed nothing answers @Just
-- []@, which 'Glance.Query.resolveColumns' serves as the title column alone.
columnNamesIn :: Text -> Either Text (Maybe [Text])
columnNamesIn q = case filter ((== Just columnsKey) . tmKey) (parseFilter q) of
  []     -> Right Nothing
  tokens -> do
    asked <- concat <$> traverse segmentsOf tokens
    pure $ case asked of
      []     -> Nothing
      spoken -> Just (snd (foldl' apply (False, filterKeys) spoken))
  where
    -- The seed is spent ONCE: a second absolute name composes, which is what
    -- keeps `columns:state columns:title' the pair it has always been.
    apply (stated, names) (Absolute n)
      | stated    = (True, extend names n)
      | otherwise = (True, [n])
    apply (stated, names) (Add n)  = (stated, extend names n)
    apply (stated, names) (Take n) = (stated, filter (not . same n) names)
    -- The first spelling of a name wins and the later one drops, refusing
    -- nothing: a duplicate names a column the set already shows.
    extend names n
      | any (same n) names = names
      | otherwise          = names <> [n]
    same a b = T.toCaseFold a == T.toCaseFold b

-- | Every segment T asks for, or 'Left' with what is wrong with the token.
-- The two token-wide refusals are spent AHEAD of any segment: a negated or
-- alternating token states no set whatever its segments spell.
segmentsOf :: Term -> Either Text [Asked]
segmentsOf t
  | tmNegated t                 = Left (refused t "a columns key cannot be negated")
  | T.isInfixOf "|" (tmValue t) = Left (refused t "a columns list is commas, \
                                                  \and takes no alternatives")
  | otherwise = concat <$> traverse (askedOf t) (T.splitOn "," (tmValue t))

-- | What SEG asks for.  ONE leading sign is read and everything behind it is
-- the NAME, so a drawer key opening with a sign is reached as @++notes@.  A
-- sign with no name behind it asks for nothing, the way @columns:@ names
-- nothing.
askedOf :: Term -> Text -> Either Text [Asked]
askedOf t seg = case T.uncons seg of
  Nothing          -> Right []
  Just ('+', name) -> Right (asking Add name)
  Just ('-', name)
    | minimal name -> Left (refused t (quoted name <> " is the one column every \
                                       \view carries and cannot be taken off"))
    | otherwise    -> Right (asking Take name)
  _bare            -> Right [Absolute seg]
  where
    asking f name | T.null name = []
                  | otherwise   = [f name]
    minimal name = T.toCaseFold name `elem` minimalNames

-- | 'Glance.Web.Filter.refusedOn' under this reader's own key.
refused :: Term -> Text -> Text
refused = refusedOn columnsKey

quoted :: Text -> Text
quoted t = "'" <> t <> "'"
```

`src-query/Glance/Query.hs`, the two constants and the one line they replace
(`:3183-3185`):

```haskell
-- | The one column every view carries.  The GRAMMAR refuses a query asking for
-- its removal ('Glance.Web.Columns') and this module supplies it to a query
-- that never named it, so the refusal and the injection read ONE fact.
minimalKey :: Text
minimalKey = "title"

-- | Every spelling that names it, folded — its key and its header, the way
-- 'resolveColumns' matches any other name.
minimalNames :: [Text]
minimalNames = [ T.toCaseFold n | (key, header, _kind, _cell) <- viewColumns
                                , key == minimalKey, n <- [key, header] ]

resolveColumns :: [Text] -> [ViewColumn]
resolveColumns names = withTitle (map pick names)
  where
    withTitle cols
      | any (\(key, _h, _k, _c) -> key == minimalKey) cols = cols
      | otherwise = [ col | col@(key, _h, _k, _c) <- viewColumns
                          , key == minimalKey ] <> cols
    -- pick / builtins / custom unchanged.
```

## Test plan

`test/TestFilter.hs`, into `columnsSpec` (`:560`), in its idiom — `columnNamesIn`
against a string, `described (resolveColumns …)` for what a name becomes,
`refusedNaming` for a 400 (`TestDefaults.hs:435`). `viewColumns` joins the
import at `:22`.

```haskell
  , testCase "the seed is the default view, and a modifier that moves nothing keeps it" $ do
      assertEqual "a plus over a column already there"
                  (Right (Just filterKeys)) (columnNamesIn "columns:+state")
      assertEqual "a minus over a column that is not there"
                  (Right (Just filterKeys)) (columnNamesIn "columns:-nosuchcolumn")
      assertEqual "and the resolved set is the default view itself"
                  (described viewColumns) (described (resolveColumns filterKeys))

  , testCase "a plus lands at the end, a minus takes one out" $ do
      assertEqual "added last" (Right (Just (filterKeys <> ["notes"])))
                  (columnNamesIn "columns:+notes")
      assertEqual "removed, the rest keeping their order"
                  (Right (Just (filter (/= "tag") filterKeys)))
                  (columnNamesIn "columns:-tag")
      assertEqual "the header spelling names the same column"
                  (columnNamesIn "columns:-tag") (columnNamesIn "columns:-Tags")

  , testCase "the FIRST absolute name spends the seed and the rest compose" $ do
      assertEqual "an absolute set is what it always was"
                  (Right (Just ["state", "title"])) (columnNamesIn "columns:state,title")
      assertEqual "a modifier onto it composes"
                  (Right (Just ["state", "title", "notes"]))
                  (columnNamesIn "columns:state,title columns:+notes")
      assertEqual "a second absolute name composes rather than restating"
                  (Right (Just ["state", "title"]))
                  (columnNamesIn "columns:state columns:title")
      assertEqual "an absolute name after a modifier restates the whole set"
                  (Right (Just ["state"])) (columnNamesIn "columns:+notes columns:state")

  , testCase "mixing is one sequence, in a token and across tokens alike" $
      assertEqual "one token and two say the same thing"
                  (columnNamesIn "columns:+notes,-tag")
                  (columnNamesIn "columns:+notes columns:-tag")

  , testCase "'-x +x' moves the name to the end, restoring no place" $
      assertEqual "state leaves the front and joins the back"
                  (Right (Just (filter (/= "state") filterKeys <> ["state"])))
                  (columnNamesIn "columns:-state,+state")

  , testCase "the minimal column cannot be taken off, in either spelling" $
      mapM_ (\q -> refusedNaming (T.unpack q) ["title"] (columnNamesIn q))
        [ "columns:-title", "columns:-Title", "columns:state,-title"
        , "columns:+notes columns:-title" ]

  , testCase "a fold that empties serves the title alone" $ do
      assertEqual "named something, composed nothing"
                  (Right (Just [])) (columnNamesIn "columns:state,-state")
      assertEqual "and the resolver's own wall supplies the column"
                  [("title", "Title")] (described (resolveColumns []))

  , testCase "a sign with no name behind it asks for nothing" $ do
      assertEqual "a bare plus" (Right Nothing) (columnNamesIn "columns:+")
      assertEqual "a bare minus" (Right Nothing) (columnNamesIn "columns:-")
      assertEqual "and it drops out from between real names"
                  (Right (Just ["state"])) (columnNamesIn "columns:+,state,-")

  , testCase "ONE leading sign is read: the rest is the name" $ do
      assertEqual "a drawer key opening with a sign is reachable"
                  (Right (Just (filterKeys <> ["+notes"])))
                  (columnNamesIn "columns:++notes")
      assertEqual "and it resolves as a custom column, header as written"
                  [("title", "Title"), ("+notes", "+notes")]
                  (described (resolveColumns ["+notes"]))

  , testCase "the token's minus and the value's minus are two terms" $ do
      assertEqual "the token negation" [Term True (Just "columns") "state"]
                  (parsed "-columns:state")
      assertEqual "the value modifier" [Term False (Just "columns") "-state"]
                  (parsed "columns:-state")
      refusedNaming "negated" ["negated", "-columns:state"]
                    (columnNamesIn "-columns:state")
      refusedNaming "negated ahead of the segment" ["negated"]
                    (columnNamesIn "-columns:-title")

  , testCase "'->' carries no rule here: it is part of a name" $ do
      assertEqual "one name" (Right (Just (filterKeys <> ["a->b"])))
                  (columnNamesIn "columns:+a->b")
      assertEqual "and one custom column"
                  [("title", "Title"), ("a->b", "a->b")]
                  (described (resolveColumns ["a->b"]))
```

The existing "it narrows nothing, whatever it names" case (`:569-573`) takes
three more strings in its `mapM_` list — `"columns:+notes"`, `"columns:-tag"`,
`"columns:-title"` — the last showing that the refusal is the READER's and that
the matcher never sees the token (`Filter.hs:219`).

`test/TestServe.hs`, one wire case beside the existing `columns:` ones:
`/headlines?q=columns:-tag` answers five columns in the default order with no
`tag` among them, and every row's cells keyed to that set (`Query.hs:3086` fills
cells off the same list it declares).

## LOC estimate

| file | change |
|---|---|
| `src-web/Glance/Web/Columns.hs` | +55 / −12 (65 → ~108, half of it the header) |
| `src-query/Glance/Query.hs` | +9 / −2 |
| `test/TestFilter.hs` | +62 |
| `test/TestServe.hs` | +8 |
| `CLAUDE.md`, `CHANGELOG.md` | +8 |
| `../table-view/SCHEMA.md` | +9 (cross-repo) |

**≈ +150 new, ~15 changed, 0 in the renderer.** Marginal cost of a third
segment kind (none is wanted): one `Asked` constructor, one `askedOf` arm, one
`apply` equation.

## Risk

**Low, and confined to the grammar layer.** The wire, the routes, the resolver's
shape and both renderers are untouched. Every query that parses today parses to
the same answer, with one exception stated above: a bare name opening with `+`
or `-` now reads as a modifier, and `++name` is where it moved.

Two things worth watching:

- **The seed identity.** `resolveColumns filterKeys` must equal `viewColumns`,
  or every modifier-only query silently re-shapes the default view. It holds by
  construction (`Query.hs:3186-3188`) and the first test case asserts it, which
  is the case that fails loudly the day a column is appended with a header
  colliding with another's key.
- **`Just []` becoming reachable.** Today it cannot arise; under the fold it
  can, and the meaning is deliberately DIFFERENT from `Nothing` — the title
  alone rather than the default view. `Routes.hs:326` reads `Maybe` and needs no
  edit, since `resolveColumns []` is total.

`-Werror=incomplete-patterns` covers `Asked`: `apply` spells its three arms and
`askedOf`'s wildcard is over `Char`, which is no closed sum.

## Existing precedent

- **The fold itself.** `Columns.hs:42-52` is already `foldl` + first-wins
  `extend`; this changes the seed and grows the step.
- **Segments as one sequence.** `sort:`'s `->` is read where a whole token's
  value is, "so no rule below knows which spelling it came from"
  (`Sort.hs:107-110`, CLAUDE.md). The comma is that same sugar, older.
- **Coarsest refusal first, all ahead of a byte.** The capture path's rule,
  spelled here as negation → alternation → segment.
- **A no-op costs nothing.** `setPriorityEdits` clearing a headline that carries
  no priority, `removeTagEdits` over a tag that is not there — both idempotent
  from the absent side.
- **Two layers, one fact.** `settingOf key` / `settingEdits key` reading one key
  constant (CLAUDE.md's config bullet) is `minimalKey`'s shape one module over.
- **The producer is the stricter side.** Every `sort:` refusal is producer-only
  and SCHEMA.md blesses the asymmetry; `columns:`' two refusals already are, and
  the third joins them.

## Open decisions

1. **Whether an absolute name written AFTER a modifier should be a 400 rather
   than a silent restatement.** `columns:+notes columns:state` answers `[state]`
   under this proposal and the `+notes` is spent with nothing said — the one
   place in this grammar where written order discards a reader's own token. The
   refusing alternative has a precedent word for word: `sort:*none*` "ADMITS NO
   COMPANIONS … a reader who wrote both meant one of them" (`Sort.hs:27-31`).
   The cost of refusing is that a saved view's absolute token plus a hand-typed
   modifier becomes order-sensitive in a way a reader composing a query in a
   text box will hit. **A human takes this one.**
2. Whether the renderer's completion gets the sign-aware prefix (§6) now or
   never. It is ~4 lines in `../table-view` and nothing owes it.
3. Whether `columns:` should also gain `*none*`, the meta that spells "the
   minimal set and nothing else". `columns:-state,-priority,-scheduled,
   -deadline,-tag` says it today; the meta would say it in one token, and the
   starred family is total by construction (CLAUDE.md), so a new member is a
   real addition rather than a spelling.
