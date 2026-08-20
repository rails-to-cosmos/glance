# Proposal — one row per column, and the five ladders dissolve

**Status:** proposed · **Date:** 2026-08-15 · **Origin:** `/simplify-repo` and
`/generalizer`, run the same night; the simplification angle and the
abstraction-shape angle found this independently and described the same defect.

## The claim `viewColumns` makes, and does not keep

`src-query/Glance/Query.hs:1798` carries the docstring **"ONE TABLE, so the four
that must agree cannot drift."**  Four do agree.  Three more per-column facts
live outside it, each dispatching on the `Text` key against a literal:

| where | the ladder |
|---|---|
| `Query.hs:1844-1850` | `case key of "state" -> badges…; "priority" -> …; "tag" -> multi` |
| `Query.hs:634-640` | `rank value = if key == "state"…`, `text' \| key == "priority"` |
| `Filter.hs:256-271` | `\| key == "state"` / `\| key == "priority"` / `\| key elem dateKeys` |
| `Filter.hs:68-69, 276-277` | `dateColumns = mapMaybe (\`elemIndex\` filterKeys) dateKeys` |
| `Base.hs:107-111` | `docCells` — a fourth, independent 4-entry table |

`ViewColumn` is `type ViewColumn = (Text, Text, Text, HeadlineRecord -> Maybe Text)`,
a 4-tuple every consumer destructures with wildcards (`:1816`, `:1817`,
`:1819-1820`, `:1835`, `:1838`, `:1842`).  Because the tuple carries no
behaviour, the behaviour scattered.

**The spec already has the shape the implementation lacks.**  `AGENTS.hs:2126`
models it as total functions over a closed sum — `data Col = CState | CPrio | …`
with `extras :: Col -> [Extra]` and `matchOf :: Col -> Match` (`AGENTS.hs:2266`)
— and `AGENTS.hs:2540` records the ladders as hand-maintained.  Nothing compares
the model's `columnNamesIn` to the real one, so the spec would rot in silence.

## What it costs today

Adding a seventh column is one row plus four arms in three modules, none of them
compiler-checked.  Two consequences are live today:

- **`Base.docCells` spells `"tags"` where the view spells `"tag"`**, and it
  crosses the port (`Page/Glue.hs:15` → `20-sheet.js:5`), so the browser learns
  a four-name column vocabulary while the table wire learns a six-name one.
- **`Filter.cellsTest` falls to `T.isInfixOf` for an unrecognized key**, so a
  column whose match rule was forgotten matches by substring and looks like it
  works.

One latent asymmetry surfaces the moment the ladders become fields:
`Filter.hs:271` applies `priorityLetter` (bracket-stripping) to the **state**
column, where the spec's `matchOf CState = MWhole` asks only for folded
whole-value equality.

## Proposed change

`ViewColumn` becomes a record carrying its own decoration and its own match
rule:

```haskell
data Decor = Plain | Badges | MultiValue deriving (Eq, Show)

data ViewColumn = ViewColumn
  { vcKey    :: !Text
  , vcHeader :: !Text
  , vcSort   :: !Text                        -- what the sort reader ranks on
  , vcCell   :: !(HeadlineRecord -> Maybe Text)
  , vcDecor  :: !Decor
  , vcMatch  :: !(Text -> Text -> Bool)      -- what `cellTest` switches on today
  }
```

`columnsFor`'s `extra`, `sortCell`'s `rank`/`text'` and `Filter.cellsTest`
become field reads.  `docCells` becomes `filter vcInDoc viewColumns` or a marked
subset, so the two vocabularies become one and the `tag`/`tags` split is gone.

**Precedent, in the same file:** `ConfigSetting { csName, csScope, csEdits }`
(`Query.hs:1710-1714`) with `configSettings` as its registry and `settingsFor`
dispatching on it — a registry entry carrying its own edit function rather than
a `case` over names.  Also `Filter.hs:211` and `:244`, whose comments state the
discipline outright: *"ONE EQUATION PER CONSTRUCTOR and no wildcard, so a fifth
key is named HERE by the compiler."*

## Files

`src-query/Glance/Query.hs` (the type, `viewColumns`, `columnsFor`, `sortCell`),
`src-web/Glance/Web/Filter.hs` (`cellsTest`, `dateColumns`, `tagsColumn`),
`src-web/Glance/Web/Base.hs` (`docCells`), `AGENTS.hs` (`Col`/`extras`/`matchOf`
gain a `TestSpec` comparison against the real registry), `test/TestFilter.hs`,
`test/TestQuery.hs`, `test/TestSpec.hs`.

## LOC

Added ~35 (the record and its rows).  Removed ~45 now (five ladders).  **Saved
per future column: ~20 lines across three modules, and four unchecked
registration steps become zero.**

## Risk

Behaviour-neutral if transcribed, with one deliberate exception: the state
column's match narrows from substring-after-bracket-stripping to folded whole
equality, which is what the spec already says.  That is a user-visible filter
change and wants its own decision.  No wire field moves; no org bytes change.

## What would say this was wrong

The decoration and the match rule turn out to vary by *view* rather than by
column — a column that badges in the table and plains in the doc pane.  Then
`Decor` belongs to the consumer, not the registry, and only `vcMatch` moves.
Check the two consumers before writing the record.
