# Bug — a rich draft opens with no editor

**Status:** fixed · **Reported:** 2026-08-24 (a capture opened with point on
the `%?` row and nothing to type into) · **Surface:** the capture doc ·
**Fixed in:** `frontend/glue/20-sheet.js`

## Symptom

The **bare** draft opened in its title box, which is the bare-draft law. Every
other draft opened with an editor nowhere:

- a template with content landed point on the row `%?` stood in — drawn,
  selected, inert — and the reader had to press `RET` before they could type;
- a draft the filter lent a fact (`priority:[#A]`, say) has `point: null`, so
  point rested on the headline with no title box either, and it was no longer
  bare so the title edit never opened.

A reader who pressed `+` is composing a capture. The pane made them answer a
question `+` had already answered.

## Steps to reproduce

Serve `test/browser/tree`, whose `book` layer spells `Why this one: %?`.

1. Press `+`, type `book`, press `RET`. Point is on `Why this one: `,
   `#dpara` is not `on`, `document.activeElement.id` is `""`.
2. Filter to `priority:[#A]`, press `+`, type `bicycle`, press `RET`. Point is
   on the head row, `#dtitle` is not `on`.

## Evidence

- `frontend/glue/20-sheet.js:2463` (`showDraft`) — the open branched on
  `bareDraft(editing)` alone: the bare draft got `openTitle`, and every other
  draft got a word in the echo and no editor at all.
- The row `%?` named lands a **macrotask** behind the fill (`docPane`'s
  `docState` subscription), so an editor over it could not be opened inline
  beside the `dsend({kind:"fill"})` that placed it.

## Fix

**Every draft opens editing**, at the place `%?` named.

`showDraft` (`:2463`) reads the answer's own `point`: `null` is the headline
and the title edit opens at once, off the handle's cells, owing the pane's rows
nothing. A body line arms `dlanding`, and `openLanding` (`:177`) — run from the
`docState` settle, where the rows are drawn — opens the paragraph editor over
the row the walk landed on, seeded from that same row. The caret rests at the
line's end: `point` names a line and there is no offset to aim at. A row no
editor claims (a template's own child headline) keeps point, and the pane's
`RET` is the way in as on any doc.

**The bare-draft law is untouched.** `+`, `RET`, the line, `RET` still lands the
inbox jot byte for byte. The **destination tag** was newly on the draft's tag
cell and would have made every tagged draft rich, so `bareDraft` (`:156`) now
asks `tagsBeyond` (`:165`) — the run **apart from** the destination — because
the destination is the address `+` already asked for and not a fact the
template or the filter brought. A bare template under a tag is still the bare
draft, so the tagged jot is four keys too.

On a rich draft the box the landing opened is an **ordinary sheet edit**, so
the standing ladder holds and nothing about it is special: `RET` closes it and
`C-c C-c` behind it takes the capture; `ESC` closes it and the next `ESC` drops
the draft.
