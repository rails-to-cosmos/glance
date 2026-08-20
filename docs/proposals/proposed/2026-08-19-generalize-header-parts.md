# Proposal — the header is a list of parts, and the third part is already lifted

**Status:** proposed · **Date:** 2026-08-19 · **Origin:** `/generalizer` audit over
`a74685c` (the subtree document) and `4170ca1` (the harvest), the cross-cut and
next-variant angles.

## The finding, in one line

The lifted header is exactly two parts — planning and the properties drawer —
and the pair is enumerated by hand at ~20 sites in five languages; the server
already lifts a THIRD part (`hpLogbook`, `Query.hs:777`), the shell already
draws it inert (`drawLog`, `20-sheet.js:1029`), and the spec already names its
promotion (`rowed ALogbook = False`, `AGENTS.hs:4075-4082`) — so the next
variant is scheduled, and today it costs a sweep of every one of those sites.

## The pattern

`4170ca1` collapsed the Elm-side spelling into ONE `headerJSON`
(`Doc.elm:784-791`) — three consumers (`stateJSON`, `cargoJSON`, `docTook`)
now splice the same two fields.  That move stopped at the module boundary.
Everywhere else the pair is still two parallel names:

| where | the twin spelling |
|---|---|
| Elm model | `props` / `plan` / `planKeys` fields, `Doc.elm:79-81` |
| Elm fill | two decoder fields + `planKeys`, `Doc.elm:854-859` |
| Elm stash | `SetMeta props plan`, `Doc.elm:271`, `:384`, `:974-977` |
| Elm delete | `keptPlan` / `keptProps`, `Doc.elm:420-439` |
| Elm edit | `editMeta`'s two arms, `Doc.elm:637-662` |
| Elm rows | `metaRows plan props`, `Body.elm:225-260`; ids `Body.elm:91-108` |
| glue mirror | `dprops` / `dplan`, `20-sheet.js:7`, `:28` |
| glue write | `commitDocWith` names both, `20-sheet.js:544`; `asked()`, `:755-757` |
| glue fill | `props:` / `plan:` / `planKeys:`, `20-sheet.js:566-568` |
| glue dirt | `stamp(props, plan)` at `:800`, `:1016`, `:1019-1020` |
| wire types | `DocState` / `DocCargo`, `glue.d.ts:53-61` |
| server read | `subtreeJSON` two pairs + logbook, `Routes.hs:403-405` |
| server write | `SplitSubtree !Text ![(,)] ![(,)]`, `Routes.hs:716-720`, `:733-736` |
| server compose | `[plan, props, logs]` region list, `Query.hs:809` |
| spec | `PRow` ids `AGENTS.hs:4057-4061`, `taken` `:4068-4073` |

The Meta arm scatters with it: `Doc.elm` dispatches on `kind == Meta` at twelve
sites (`240, 453, 519, 594, 891, 1034, 1271, 1514, 1523, 1533, 1552, 1657`),
`Body.elm` at two (`679`, `963`), the glue at two (`20-sheet.js:77`, `:243`) —
each one an implicit member test against "the synthesized header rows", a set
that grows with every part.

Rowing the logbook on today's shape touches all fifteen rows of the table plus
`TestServe.hs` and `cases.mjs`: a five-file sweep is the floor, eight files the
likely count.

## Files

`frontend/elm/src/Doc.elm`, `frontend/elm/src/Body.elm`,
`frontend/glue/20-sheet.js`, `frontend/glue.d.ts`,
`src-web/Glance/Web/Routes.hs`, `src-query/Glance/Query.hs`, `AGENTS.hs`,
`test/TestServe.hs`, `test/browser/cases.mjs`.

## Proposed change

One list of parts, indexed by key, each carrying its own verbs — the `Sheet`
move (`AGENTS.hs:3557-3575`) applied to the header.

**Elm** (`Body.elm` owns the list):

```elm
type alias Part =
    { key : String                              -- the wire word: "planning", "properties"
    , frame : Maybe String                      -- Just "PROPERTIES": a framed drawer; Nothing: a bare line
    , showPair : ( String, String ) -> String   -- propertyText / planning's "KEY: v"
    , readLine : String -> Maybe ( String, String )
    , onTake : TakenAs                          -- Cleared (planning) / Dropped (a pair)
    }

parts : List Part                               -- today two entries; the logbook is a third

partId : Part -> String                         -- "PLN", "PR" — minted off the key
pairId : Part -> Int -> String
```

The model holds `header : List ( String, List ( String, String ) )` keyed the
same way; `metaRows`, `remeta`, `editMeta`, `AddProp`, the delete's `kept*`
and `landOn`'s drawer fallback (`Doc.elm:615`) all fold over `parts` and look
an id up through the one list — the twelve `kind == Meta` sites shrink to the
few that mean "synthesized" (span, splice, join), answered by one
`synthetic : Row -> Bool`.  `viewMeta`'s literal `":PROPERTIES:"`
(`Doc.elm:1568-1573`) and `crumb`'s re-derivation (`:1645-1649`) read the
part's `frame`.

**Glue**: one mirror.  `dheader = now.header` (an object keyed by part);
`asked()` returns `{ body: dbody, ...dheader }`; `stamp` takes the one object;
`commitDocWith` spreads `cargo`.  The HTTP wire keeps its flat field names —
the spread reproduces today's `properties` / `planning` byte for byte — so the
POST body drifts only when a third part actually ships.

**Server**:

```haskell
data Commitment = WholeSubtree !Text | SplitSubtree !Text ![(Text, [(Text, Text)])]

liftedParts :: [(Text, HeadlineParts -> [(Text, Text)])]   -- the ONE list, keyed
```

`parseCommit` reads each key `liftedParts` names; `subtreeJSON` emits from the
same list; `recomposedSubtree` already composes from a region list
(`Query.hs:809`).  `badPlanning` stays the planning entry's own verb.

**Spec**: `PRow` gains the part as a parameter (`Pair Part Int`); `rowed`
flips to `True` per part as each ships.

## LOC estimate

+70 now (the `Part` record and its two entries, the keyed fold sites,
`liftedParts`), −65 (the twin arms, the twin mirrors, the twin kept/edit
paths).  Per future part: ~8 lines Elm (one record) + ~3 lines Haskell (one
`liftedParts` entry) + its own tests; the glue, `glue.d.ts` and the wire
plumbing go untouched.  Today the same part costs ~70 lines over eight files.

## Risk

The page-internal fill/state port shape changes (`header` as one keyed value);
`TestServe` funnels commit bodies through `splitBody` / `planningBody`
(`TestServe.hs:162-166`), so the POST-side baseline concentrates there.  The
HTTP wire is unchanged until a new part ships.  The parts genuinely differ —
planning clears where a pair drops, its line reparses under
`planningKeywords` — and the record carries those verbs; a part whose
behaviour will not fit the record (the logbook's CLOCK lines read differently)
is the test of whether the record's verbs are the right ones, and `readLine`
per part is where that difference lives.

## Existing precedent

`headerJSON` itself (`Doc.elm:784-791`, minted by `4170ca1`) is this proposal
executed one module wide.  The sheet object (`20-sheet.js:769-784`,
`AGENTS.hs:3557-3575`) and the flag shape (`FLAG_WORDS` / `DFLAGS` / `XFLAGS`)
are the same move; `docs/invariants.md` ("a fact several readers agree on is
spelled in ONE list, indexed by key") names it as the repo's shape.
