# Proposal — the marker's facts ride the row, and the grammar is spelled once

**Status:** proposed · **Date:** 2026-08-19 · **Origin:** `/generalizer` audit over
`a74685c` and `4170ca1`, the cross-cut angle; the shell's own comments claim
"this page spells no org" (`20-sheet.js:224`, `:831`) and three regexes spell it.

## The finding, in one line

The checkbox vocabulary is spelled four times — `boxLen` (`Doc.elm:1362-1368`),
`boxAfter` (`Body.elm:530-537`), the `CHECKBOX` regex (`20-sheet.js:159`),
`boxes` (`AGENTS.hs:3786-3787`) — and the bullet grammar three times
(`Scan.listOpener` `Scan.elm:47-89`, `OPENER` `20-sheet.js:175`, `CONT`
`:177`), so a new marker form is a five-site edit across three languages that
nothing joins: a form Elm renders and a JS regex misses toggles nothing,
silently.

## The pattern

Org has marker forms the pane will meet: the `[@4]` counter, description items
(`term ::`), and `flipBox`'s four-state cycle is already spec
(`AGENTS.hs:3776-3787`).  Each lands today in:

| spelling | reader |
|---|---|
| `Scan.listOpener` + `indentOf` | the real grammar, `Scan.elm:47-89`, `:346-348` |
| `Doc.boxLen` | the drawn box span, `Doc.elm:1358-1368` |
| `Body.boxAfter` | a sibling's fresh box, `Body.elm:530-537` |
| `CHECKBOX` | `toggleCheckbox` rewrites the row's text in JS, `20-sheet.js:159-170`; `checkboxHere` `:587` feeds `C-c C-c` (`70-shell.js:151-152`) |
| `OPENER` / `indentOf` | `rungsFor` reads model rows' indents, `20-sheet.js:175`, `:182`, `:187-201` |
| `CONT` | `newlineIn` re-parses the box's first line, `20-sheet.js:177`, `:460-471` |

The repo already fixed this shape for words: the region word rides `docSaid`
because "the rule lives once, where the model is" (`AGENTS.hs:3811-3817`,
`Doc.elm:538-543`).  The marker's FACTS deserve the same door as its words.

## Files

`frontend/elm/src/Scan.elm`, `frontend/elm/src/Body.elm`,
`frontend/elm/src/Doc.elm`, `frontend/glue/20-sheet.js`,
`frontend/glue/70-shell.js`, `frontend/glue.d.ts`, `AGENTS.hs`.

## Proposed change

One spelling in Scan, the facts on the wire row, the toggle a message.

**Scan** owns the vocabulary once:

```elm
boxAt : String -> Maybe String   -- the box token after an opener, or Nothing
```

`Doc.boxLen` and `Body.boxAfter` both read it; the two `[ ] [X] [x] [-]`
lists collapse to one.

**The row carries what the shell reads off it.**  `rowJSON` gains:

```elm
, ( "box",    Maybe.withDefault E.null (Maybe.map E.string (boxOf m r)) )
, ( "marker", E.int (markerLen m r) )   -- markerLen exists, Doc.elm:1319-1321
```

`DocRow` (`glue.d.ts:29-42`) grows the two fields.  Then in the glue:
`checkboxHere = () => (docRowAt() || {}).box ?? null` — `CHECKBOX` and
`checkboxAt` die; `rungsFor` reads indents off `r.marker`-bearing rows in
place of `OPENER`+`indentOf` over `r.text`.

**SPC becomes a message.**  `{kind: "flipbox", id}` → Doc flips the box by
`flipBox`'s own table through the existing `Edit` path and answers over
`docSaid`; `toggleCheckbox`'s regex rewrite dies and `70-shell.js`'s
`C-c C-c` arm sends the same message.

**The boundary, stated:** grammar over LIVE TYPED TEXT stays the box's own —
`newlineIn`'s continuation indent and `tabRung`'s first-line read
(`20-sheet.js:202-218`) parse what the reader may just have retyped, and a
model round trip mid-keystroke buys nothing.  `CONT` and the rung walk stay;
`CHECKBOX` and the model-row reads go.  After the change the shell parses org
only inside an open textarea, which is what its comments already claim.

## LOC estimate

+25 now (`boxAt`, two row fields, the `flipbox` arm), −30 (`CHECKBOX`,
`checkboxAt`, `toggleCheckbox`'s rewrite, the model-row `indentOf` reads, one
duplicate box list).  Per future marker form: `Scan` + the spec + a browser
case; today the same form costs five spellings and an unjoined sixth in the
spec.

## Risk

Page-internal wire only — `rowJSON` grows two fields, `glue.d.ts` once; the
HTTP wire is untouched.  The flip's semantics are already identical on both
sides (`flipBox` and the JS ternary agree case for case).  `docSaid`'s
one-shot `dwrote` handshake gains one more sender; the two-answer disarm
pattern (`20-sheet.js:266-273`) is the template.

## Existing precedent

The grain word (`docSaid`, `AGENTS.hs:3811-3817`): the model answers, the
shell echoes.  The `fold` field on the row (`Doc.elm:723-724`, read at
`20-sheet.js:76`) is a model fact already riding the row for the shell to
spend.  `../done/2026-08-18-generalize-port-kind-join.md` joined the port
vocabulary across the same language boundary.
