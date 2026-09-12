# Proposal — the door hands over values, and a record beats positional twins

**Status:** proposed · **Date:** 2026-09-12 · **Origin:** `/generalizer`, the
cross-cut sweep over the web layer — four instances of one shape found in one
pass.

*`Routes.hs` line numbers are read against the working tree of 2026-09-12.*

## Pattern

One shape, four sites: **a field carries less than the thing it was resolved
from, or more**, and the gap is held shut by a convention rather than by a type.

### (a) `Focus` carries the whole `Store` so one line can read `stEdges`

`Focus` (`Routes.hs:530-535`) is `{fcStore :: !Store, fcRow, fcEntries, fcAt}`.
Nine renderers take a `Focus` — `focusEntry` `:587`, `focusHere` `:590`,
`subtreeJSON` `:597`, `docPairs` `:618`, `levelOf` `:631`, `firstUnder` `:653`,
`beneath` `:659`, `upFrom` `:669`, `trailTo` `:672` — and **not one reads
`fcStore`**. The single reader is `materialize`'s rider, one line:
`edgePairs (stEdges (fcStore f)) (fcRow f)` (`Routes.hs:527`).

So every pure renderer of a subtree carries a live store version, and the
"record retains no document bytes" discipline (`docs/invariants.md`) has a
counterweight one level up: the focus retains the whole world. `draftJSON`
(`Routes.hs:842-861`) shows the cost — a fileless draft has no store row at all,
yet it must build `Focus st r (subtreeEntries …) Nothing` (`:860`) to call
`docPairs`, handing a draft a store it has no use for.

### (b) `Asks` is a tag beside parallel `Maybe` fields in `Asked`

`Asked` (`Commands.hs:113-117`) is `{askToday :: !Time.Day, askStamp ::
!(Maybe Text), askLink :: !(Maybe Text)}`, and `Asks`
(`Commands.hs:122`) is `AsksNothing | AsksDate | AsksLink` — the declaration of
which of those two `Maybe`s the door will fill (`resolveAsked`,
`Commands.hs:348-351`). The two are joined by convention only. A command
declaring `AsksDate` whose edit set reaches for `askLink` compiles and appends
an empty link; a command declaring `AsksNothing` that reads either gets
`Nothing` and writes whatever `fromMaybe ""` gives it. `add-link`'s edit
already spells `word askLink asked` (`Commands.hs:155`) to paper over exactly
this.

The docstring at `Commands.hs:119-121` states the intent — *"a closed word
rather than a flag apiece, so a fourth request-level value is named by the
compiler at `resolveAsked`"* — and the compiler names it at `resolveAsked`
alone. Nothing names it at the read.

### (c) `SplitSubtree` is `HeadlineParts` minus the logbook, spelled positionally

`Commitment` (`Routes.hs:1053-1057`):

```haskell
  | SplitSubtree !Text ![(Text, Text)] ![(Text, Text)]
      -- ^ @body@, @properties@ and @planning@, to be composed.
```

`HeadlineParts` (`Query.hs:1044-1049`) is the same three plus `hpLogbook`, each
named. The two same-typed lists are positional, so swapping them typechecks and
writes the planning entries into the drawer. `committed` then converts one to
the other by hand: `recomposedSubtree doc r (HeadlineParts body ps pln "")`
(`Routes.hs:709-712`), and `settledPlanning` (`:718-723`) rebuilds the
constructor to touch the third field.

Beside it, **the `[[key, value]]` pair parser is spelled twice, with two
different refusal sentences**:

- `Commands.hs:566-568` — `cargoPairs`, refusing with *"each planning entry and
  property is a [key, value] pair"*
- `Routes.hs:1074-1075` — `pair`, refusing with *"each property is a [key,
  value] pair"*

Both read the same wire shape, described in `Commands.hs:564-565` as *"one
spelling of the doc pane's two lists, so a draft and a row edit agree."* It is
two spellings.

### (d) `Args` is 19 fields filled positionally

`Args` (`Commands.hs:59-80`) has 19 fields, ten of them `Maybe Text`
(`agText`, `agTitle`, `agTag`, `agState`, `agBody`, `agFrom`, `agTo`,
`agTarget`, `agKind`, `agWhere`). `parseCommand` fills it positionally across
nine lines (`Commands.hs:553-561`):

```haskell
      parsed <- Args <$> a .:! "keyword" <*> a .:! "date" <*> a .:? "text"
                     <*> a .:? "title" <*> a .:! "priority" <*> a .:? "tag"
                     …
                     <*> a .:? "kind" <*> a .:? "where"
```

Any two of the ten `Maybe Text` positions can be transposed and the module still
compiles. `from`/`to` sit adjacent (`:559`) and `target`/`kind`/`where` nearly
so (`:560-561`); a swap there renames a tag backwards or writes a link's kind as
its description. `LayerWrite`'s own docstring
(`Routes.hs:1010`) reads *"A record: three of the four are
`Text`, so a transposed pair would compile."* That admission exists; the
19-field record has none.

## Files

`src-web/Glance/Web/Routes.hs:512-535`, `:576-676`, `:705-725`, `:842-861`,
`:1010-1017`, `:1053-1076`; `src-web/Glance/Web/Commands.hs:59-80`,
`:113-122`, `:149-160`, `:340-360`, `:551-575`; `src-query/Glance/Query.hs:1044-1056`;
`src-web/Glance/Web/Base.hs:204-207`.

## Proposed change

Four independent edits; any one lands alone.

**(a) `Focus` carries the graph it owes.**

```haskell
data Focus = Focus
  { fcEdges   :: EdgeIndex         -- ^ the graph the id resolved in; `edgeIndex []` for a draft.
  , fcRow     :: !HeadlineRecord
  , fcEntries :: ![SubtreeEntry]
  , fcAt      :: !(Maybe Int)
  }
```

`focused` (`Routes.hs:578-585`) sets `fcEdges = stEdges st`, which stays the
lazy per-store-version value `stEdges` already is. `materialize`'s rider becomes
`edgePairs (fcEdges f) (fcRow f)`. `draftJSON` (`:860`) passes `edgeIndex []` —
a draft is no row and has no edges, which the type then says. `stConfig` is read
at the two build sites and nowhere under them.

**(b) `Asked` carries what was asked for, in one field.**

```haskell
-- | WHAT THE DOOR RESOLVED, carried as the value it resolved to.  A command
-- that asked for nothing cannot read a date, and one that asked for a date
-- cannot read a link: the constructor IS the declaration.
data Resolved = NothingAsked | DateAsked !(Maybe Text) | LinkAsked !Text

data Asked = Asked
  { askToday :: !Time.Day
  , askFor   :: !Resolved
  }
```

`csAsks :: Asks` stays as the REQUEST's declaration — what the door must go and
resolve before any row (`resolveAsked`, `Commands.hs:348`) — and `Resolved` is
its ANSWER. The row edits pattern-match:
`RowEdits` arms take `DateAsked stamp` or `LinkAsked link` and the compiler
refuses the other. `add-link`'s `word askLink asked` (`:155`) disappears with
the `Maybe`.

**(c) `SplitSubtree` takes the record, and the pair parser moves to `Base`.**

```haskell
data Commitment
  = WholeSubtree !Text
  | SplitSubtree !HeadlineParts   -- ^ @body@, @properties@, @planning@; no logbook.
```

`parseCommit` (`Routes.hs:1060-1076`) builds
`SplitSubtree <$> (HeadlineParts <$> o .: "body" <*> pairsUnder o "properties"
<*> pairsUnder o "planning" <*> pure "")`, `committed` (`:709`) stops
converting, and `settledPlanning` (`:718`) updates `hpPlanning` by name.

One parser, in `Base.hs` beside `bodyObject` (`:204-207`):

```haskell
-- | @[[KEY, VALUE], …]@ under KEY: the shape the doc pane's two lists ride in,
-- ONE spelling and ONE refusal, so a draft and a row edit agree to the sentence.
pairsUnder :: Object -> Key -> Parser [(Text, Text)]
pairsUnder o key = traverse pair =<< o .: key
  where pair [k, v] = pure (k, v)
        pair _other = fail "each entry is a [key, value] pair"
```

with `cargoPairs` (`Commands.hs:566`) becoming its optional sibling.

**(d) `Args` is parsed by field name.**

```haskell
      parsed <- do
        base <- pure emptyArgs
        keyword  <- a .:! "keyword"
        …
        pure base { agKeyword = keyword, agDate = date, … }
```

or, keeping the applicative shape, a `parseArgs :: Object -> Parser Args` whose
every line names its field. Either way a transposition becomes a type error or a
duplicate-field error. `emptyArgs` also removes the `mempty`-shaped default at
`Commands.hs:552`.

## LOC estimate

- (a) added ~2, removed ~2 — a rename plus one `edgeIndex []`. It buys a type,
  not lines.
- (b) added ~14 (`Resolved`, three `resolveAsked` arms), removed ~10 (two
  `Maybe` fields, `word`, the `fromMaybe` sites).
- (c) added ~10 (`pairsUnder`), removed ~22 (the positional constructor, the
  hand conversion in `committed`, `settledPlanning`'s rebuild, one of the two
  pair parsers and its second refusal sentence).
- (d) added ~22 (named binds plus `emptyArgs`), removed ~9 (the positional
  chain).

**Per future command argument:** one named line rather than a position counted
against 19 neighbours. **Per future commitment part:** one `HeadlineParts`
field, already named, instead of a fourth positional list. **Per future
request-level resolution:** one `Resolved` constructor the compiler demands at
every read, rather than a fifth `Maybe` on `Asked`.

## Risk

- **API**: `Focus`, `Commitment` and `Asked` are module-internal to
  `Routes.hs`/`Commands.hs`. `HeadlineParts` is exported from `Query.hs:14` and
  gains a second consumer; its shape is unchanged. `pairsUnder` is a new
  `Base.hs` export.
- **Wire**: (c) changes ONE refusal sentence — `POST /headline`'s
  *"each property is a [key, value] pair"* becomes *"each entry is a [key,
  value] pair"*. `TestServe` asserts refusal text by substring in several
  places; that is the one baseline to walk. Everything else is byte-identical:
  the accepted bodies, the members, the statuses.
- **On-disk**: none. No write path moves; `recomposedSubtree` takes the same
  `HeadlineParts` it takes today.
- **Test baselines**: (d) touches the arm `TestSpec.hs:1016-1029` drives — it
  posts one null per `Spec.cArgs` entry and reads the 400's text, so a
  field-named parser must keep `.:!` where the spec says `Nul` and `.:?` where
  it says `Opt`. That case is the proof the rewrite is faithful.

## Existing precedent

- `LayerWrite`'s own docstring (`Routes.hs:1010`): *"A record: three of the four
  are `Text`, so a transposed pair would compile"* — the same worry, already
  answered with a record.
- `CommandKind` (`Commands.hs:131-138`) carries `Reads` and `RowEdits` in the
  constructor, so the dispatch reads the payload off the kind.
- `RowWrite` (`Commands.hs:96-101`): *"ONE ANSWER rather than two fields that
  must agree"* — (b) is that sentence applied to `Asked`.
- `docs/invariants.md`, *"a fact several readers agree on is spelled in ONE
  list"* — (c)'s pair parser is the two-reader case of it.
