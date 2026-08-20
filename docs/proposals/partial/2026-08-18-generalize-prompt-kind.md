# Proposal — the palette has three kinds, and should say so

**Status:** partial · **Date:** 2026-08-18 · **Origin:** `/generalizer` audit;
the audit proposed a two-valued tag, and weighing it produced a three-valued one.

## The finding, in one line

`text` and `narrow` are mutually exclusive values of one three-valued thing, and
the palette carries them as two independent booleans that no line declares
exclusive.

## The pattern

Cross the two facts actually encoded — *is there a list*, *is the field focused*:

| built by | list | field | today |
| --- | --- | --- | --- |
| `ask` (`30-capture.js:206-210`) | yes | no | `narrow` false |
| `askFrom` + `fieldMode` (`:254-261`, `:245-251`) | yes | yes | `narrow` true |
| `askText` (`:240-244`) | no | yes | `text` true |
| — | no | no | cannot exist |

So `text ≡ field ∧ ¬list` and `narrow ≡ field ∧ list`: they can never both be true,
and nothing says so. The readers reconstruct the shape by testing the flags —
`drawChoices` (`:275-311`) splits four ways, the shell's palette keydown
(`70-shell.js:233-273`) three — and on a text prompt `narrow` is `undefined`, so
deleting the `if (text) return` guard sends the press into the letters branch,
where it dies at `70-shell.js:254` on `choices.find` of undefined.

`raise`'s `cls` argument is the tag already, restated and threaded through two
functions and four call sites: `ask` passes `""`, `askText` passes `"narrow"`
while its state carries no `narrow` field at all, and `mode(cls, foot)` (`:264-268`)
does only `pbox.classList.toggle("narrow", cls === "narrow")`.

## The change

One flat tag, one switch per reader:

```js
// kind: "letters" | "narrowed" | "free"
const DRAW  = { letters: drawLetters, narrowed: drawNarrow, free: () => {} };
const PKEYS = { letters: letterKeys,  narrowed: narrowKeys, free: freeKeys };
```

`raise` takes `kind` instead of `cls` and derives the class from it, which deletes
the parameter. `PKEYS.free` cannot reach `shown`, so the ordering dependency that
currently holds the palette together stops being load-bearing.

## Two dead things in the same neighbourhood

Found while weighing, and worth removing whether or not the tag lands:

- `mode()` is **exported** (`30-capture.js:395`, `:411`) with no caller outside its
  own file.
- `freely()` (`:348-352`) returns null unless `prompting.wider`, which is written
  at `:258` alone — an `askFrom`, i.e. a *choices* palette. So `freely() ||` in the
  text branch (`70-shell.js:243`) is **dead**.

## LOC

~55 lines of flag-branching become ~20 of table plus three small functions. The
next palette shape is one entry in each table rather than an arm inserted at the
right depth of two `if` ladders in two files.

## Risk

The palette is on the hot path for `t`, `:`, `C-c C-s`, `C-c C-d`, `+`, `/` and the
capture form. `TestServe` drives it by keystroke and `cases.mjs` re-runs it in a
real browser, so behaviour must come out identical press for press. `raising` and
`states` are orthogonal to the kind and stay fields.

**Naming caution:** `prompting.kind` sits next to the existing function `mode()`
and next to `narrow` used as a verb elsewhere in the file; the field is `kind` and
the function goes.

## Precedent

The shell already tables its dispatch four times — `SURFACES` (`70-shell.js:16-43`),
`HANDLERS` (`:110`), `VERBED` (`20-sheet.js:900-915`), `SECTIONS`
(`50-settings.js:2-7`) — and the six edit shapes (`LROW`, `TROW`, `SROW`, `DTITLE`,
`DPARA`, `PROW`) are one record per variant. The palette is the surface left
switching on bits.
