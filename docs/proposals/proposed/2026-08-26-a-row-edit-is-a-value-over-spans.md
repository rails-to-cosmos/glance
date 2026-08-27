# Proposal — a row edit is a value over spans

**Status:** proposed · **Date:** 2026-08-26 · **Origin:** /generalizer over
the stage-1 diff — threading one `Text` argument through the row-edit
family cost 1207 changed lines, which measures the family's shape.

## Pattern

The row-editing functions in `Query.hs` (`setStateEdits`, `tokenEdits`,
`setTitleEdits`, `setPriorityEdits`, `setPlanningEdits`, `addTagEdits`,
`removeTagEdits`, `renameTagEdits`, `editLinkEdits`, `linkAtSpan`, `tagRun`,
`repeatOn`) come in three signature shapes — no document + total, document
+ total, document + refusing — and read nothing off the record but
`headlineSpans r` (and `hrSubtree`/`hrId` for wording). `Commands.hs`
normalizes the three shapes with nine hand-written adapter lambdas
(`\_cfg _asked args doc r -> plain …` / `plain =<< …`), each discarding two
to four of five positional arguments; `type RowEdits` grew its arity for
the document instead of a record. `draftSeeded` has to MINT a fake record
(`recordOf cfg declared "" 0 doc "" …`) purely to reach the family over
bytes with no file behind them. `Text -> HeadlineRecord` sits adjacent in
~35 signatures across Query/Routes/Filter and ~65 test lambdas. Adding a
row command touches six files, three of them unenforced (`VERBED` falls
back silently, `draftWrote` re-implements the server's tag rules in JS,
`docs/commands.md` is a hand table).

## Files

- `src-query/Glance/Query.hs:1438-1810` (the family), `:2225-2370`
  (drafts), `:849-1101` (the lens over a document).
- `src-web/Glance/Web/Commands.hs:104-168` (`RowEdits`, the `commands`
  table, `stateEdits`/`repeating` special-cased in `where`), `:172-215`
  (`wantsX`), `:503-510` (`parseCommand`, 17 positional `Maybe Text` slots).
- `frontend/glue/20-sheet.js:2295-2337` (`VERBED`, `draftWrote`).
- `docs/commands.md:33-45`; `AGENTS.hs` `cmds` (enforced by TestSpec).

## Proposed change

Core over spans, record wrappers one line each — the diff's own
`addTagEditsIn` and `drawerSlice` splits generalized:

```haskell
-- Glance.Query
type RowEdit = Text -> HeadlineSpans -> Either Text [(Span, Text)]
total :: (Text -> HeadlineSpans -> [(Span, Text)]) -> RowEdit
setTitleEdits    :: Text -> RowEdit
setPlanningEdits :: Text -> Maybe Text -> RowEdit
removeTagEdits   :: Text -> RowEdit           -- = total (…)
addTagEdits      :: Text -> RowEdit           -- reads the cell it already reads
-- a record's edit is the family applied to its spans:
onRow :: RowEdit -> Text -> HeadlineRecord -> Either Text [(Span, Text)]
onRow f doc r = f doc (headlineSpans r)

-- Glance.Web.Commands: one request record, one row record, one lifter
data Asked    = Asked { askCfg :: ConfigLayers, askToday :: Day, askStamp :: Maybe Text, askArgs :: Args }
data Standing = Standing { sdDoc :: !Text, sdRow :: !HeadlineRecord }  -- minted by `pinned' alone
type RowEdits = Asked -> Standing -> Either Text RowWrite
edits :: (Args -> RowEdit) -> RowEdits
commands = [ ("set-title",  … (Splices (edits (setTitleEdits . word agTitle))))
           , ("remove-tag", … (Splices (edits (removeTagEdits . tagOf)))) … ]
```

`draftSeeded` calls the span forms over the parse it already holds — no
fake record. `Standing` is not a record field, so residency is unchanged,
and the text exists only downstream of a digest check (the invariant's
"a reader that reaches for a field instead of its argument" closes by
type). `VERBED` joins `commandNames` under a TestSelfContained needle;
`draftWrote`'s tag rule becomes an answer the server sends back.

## LOC estimate

+~20 (`RowEdit`, `total`, `onRow`, `Standing`, `edits`) / −~40 (nine
adapter lambdas → one-liners, the fake draft record, `stateEdits`/
`repeating` special cases folded, the 35 `Text -> HeadlineRecord`
signatures losing an argument at the call sites that hold a `Standing`).
Per new splicing command: one table line and one `RowEdit`; today a
lambda, a `plain`/`plain =<<` choice, a `wantsX` sentence, an `Args`
field, a positional parser slot, a `VERBED` entry.

## Risk

Signature churn across Query/Commands/tests, compiler-driven; no wire
change. `Standing` needs an escape for drafts (a document with no file,
digest `""`) — the span forms are that escape, so the constructor stays
`pinned`'s alone.

## Existing precedent

`addTagEdits`/`addTagEditsIn :: Text -> Text -> HeadlineSpans -> …`, called
over a fresh parse by `stampedEntry` "so capture and command cannot
disagree"; `presentPlanning :: HeadlineSpans -> …`; `Asked` itself ("a
RECORD rather than three positional Maybe Text"); `Focus` and `FilterEnv`
as per-request context; the diff's own `drawerSlice` → span-form split.

Inert until reviewed.
