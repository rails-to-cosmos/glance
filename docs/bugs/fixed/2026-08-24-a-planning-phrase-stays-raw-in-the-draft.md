# Bug — a planning phrase stays raw in the draft

**Status:** fixed · **Reported:** 2026-08-24 (`:DEADLINE: 1 oct` stood as the
words that were typed until the capture landed) · **Surface:** the capture
doc's planning line, through the pair box and the date widget alike ·
**Fixed in:** `frontend/glue/20-sheet.js`

## Symptom

In the capture doc, entering a planning value — `:DEADLINE:` / `1 oct` in the
pair box, or `1 oct` in the widget `C-c C-d` raises — left the **raw phrase**
standing in the draft's planning line: `SCHEDULED: <…> DEADLINE: 1 oct`. The
ghost had just previewed ` → <2026-10-01 Thu>` a keystroke earlier. Only after
`C-c C-c` and materializing the blob did the line show what was written.

The bytes were never wrong: the commit landed `DEADLINE: <2026-10-01 Thu>`. The
pane was showing the reader something other than what it was about to write.

## Steps to reproduce

Serve `test/browser/tree`.

1. Press `+`, type `book`, press `RET`; press `RET` to close the editor the
   landing opened.
2. `C-c C-d`, type `1 oct`. The ghost reads ` → <2026-10-01 Thu>`.
3. `RET`. The planning line reads `DEADLINE: 1 oct`.
4. `C-c C-c`. The blob reads `DEADLINE: <2026-10-01 Thu>`.

The pair box takes the same road: walk to the drawer, `+`, `DEADLINE`, `:`,
`1 oct`, `RET`.

## Evidence

- `frontend/glue/20-sheet.js:1828` (`commitDocWith`) — every model write funnels
  here, and a draft returned early: `if (capturing()) { say(cargo); return; }`.
  A row's cargo is **posted** and the server transforms the phrase at the
  planning wall, after which `reload()` redraws off that answer; a draft posts
  nothing, so nothing ever transformed it.
- `commitDate` (`:972`) and the pair box's arm of `commitDocEdit` (`:1729`)
  both send the raw text into the model — correctly, since *what travels is
  what was typed*. The gap was the redraw a draft never gets, not the send.

## Fix

`settleDraftPlan` (`:1851`) is the draft's own **stand-in for the round trip a
row makes**, run from the one choke point every model write already passes. For
each settable planning entry it reads the value with `readsDate` — the ghost's
own resolver, drift-pinned to the wall over
`test/fixtures/english-dates.json` — and writes the resolution back into the
model, so the line says what the file will hold. A value that already spells
its own answer is left alone, which is what makes the settle reach a fixed
point in one pass.

**What travels is still what was typed.** `dtyped` records the phrase behind
each entry the settle resolved, and `typedPlan` (`:1865`) puts it back at the
commit — the wall transforms **once**, against the server's clock, which is the
whole of why two resolutions against two clocks never happen. An entry a later
door moved no longer matches its record and rides as it stands. A fresh fill
empties the record: the phrases are that document's.

A phrase the resolver refuses stays raw and meets the wall's own sentence at
the commit, the sheet standing. `CLOSED` is not settable — that wall reparses
rather than resolving — so its verbatim value is untouched here as everywhere.
