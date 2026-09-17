# Proposal — the marker's facts ride the row, and the grammar is spelled once

**Status:** partial · **Date:** 2026-08-19 · **Origin:** `/generalizer` audit over
`a74685c` and `4170ca1`, the cross-cut angle; the shell's own comments claim
"this page spells no org" (`20-sheet.js:224`, `:831`) and three regexes spell it.

## The finding, in one line

The shell-side checkbox spelling is gone. The remaining marker vocabulary is
still split between `Doc.boxLen`, `Body.boxAfter`, the model spec, and the
live-editor `OPENER`/`CONT` readers, so adding a marker form still crosses
multiple sites.

## Implemented 2026-09-17

`SPC` and the material-sheet meaning of `C-c C-c` now reach one Elm
`ToggleCheckbox` action. `Doc.elm` reads the live row, refuses derived parents,
rewrites leaf boxes, and returns the write cargo. The JavaScript `CHECKBOX`,
`checkboxAt`, `checkboxHere`, and `toggleCheckbox` mirror is deleted. This
completes the proposal's checkbox-command half without adding `box` or `marker`
fields to the wire row; the model needs no help reading its own state.

The shared `Scan.boxAt` cleanup and the live-editor marker readers remain.

## The pattern

Org has marker forms the pane will meet: the `[@4]` counter, description items
(`term ::`), and the four checkbox states are already spec. The remaining
spellings are:

| spelling | reader |
|---|---|
| `Scan.listOpener` + `indentOf` | the real grammar, `Scan.elm:47-89`, `:346-348` |
| `Doc.boxLen` | the drawn box span, `Doc.elm:1358-1368` |
| `Body.boxAfter` | a sibling's fresh box, `Body.elm:530-537` |
| `OPENER` / `indentOf` | `rungsFor` reads the open row and neighbouring model rows |
| `CONT` | `newlineIn` reads the open textarea's first line |

The repo already fixed this shape for words: the region word rides `docSaid`
because "the rule lives once, where the model is" (`AGENTS.hs:3811-3817`,
`Doc.elm:538-543`).  The marker's FACTS deserve the same door as its words.

## Files

`frontend/elm/src/Scan.elm`, `frontend/elm/src/Body.elm`,
`frontend/elm/src/Doc.elm`, `frontend/glue/20-sheet.js`, `AGENTS.hs`.

## Proposed change

One spelling in Scan, with live editor parsing kept at the browser boundary.

**Scan** owns the vocabulary once:

```elm
boxAt : String -> Maybe String   -- the box token after an opener, or Nothing
```

`Doc.boxLen` and `Body.boxAfter` both read it; the two `[ ] [X] [x] [-]`
lists collapse to one.

`Doc.boxLen` and `Body.boxAfter` both read `Scan.boxAt`, collapsing their two
checkbox token lists. `rungsFor` should either consume a marker fact already
owned by the model or move as a complete open-editor operation; adding fields
to every wire row solely for the completed checkbox command is no longer
needed.

**The boundary, stated:** grammar over LIVE TYPED TEXT stays the box's own —
`newlineIn`'s continuation indent and `tabRung`'s first-line read
(`20-sheet.js:202-218`) parse what the reader may just have retyped, and a
model round trip mid-keystroke buys nothing. `CONT` and the open row's rung
reader stay at this boundary. The remaining question is how neighbouring rung
facts reach that editor without another grammar over model rows.

## LOC estimate

The completed checkbox step removed more JavaScript than it added to Elm. The
remaining `boxAt` consolidation should remove one duplicate token list; the
rung change needs a fresh estimate after choosing its boundary.

## Risk

The shared scanner change can alter insertion and rendering together, so its
tests must cover every box state and list bullet. Any rung change must preserve
the open textarea as the source for text the reader has not committed yet.

## Existing precedent

The grain word (`docSaid`, `AGENTS.hs:3811-3817`): the model answers, the
shell echoes.  The `fold` field on the row (`Doc.elm:723-724`, read at
`20-sheet.js:76`) is a model fact already riding the row for the shell to
spend.  `../done/2026-08-18-generalize-port-kind-join.md` joined the port
vocabulary across the same language boundary.
