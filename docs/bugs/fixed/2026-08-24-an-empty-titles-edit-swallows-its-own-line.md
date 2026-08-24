# Bug — an empty title's edit swallows its own line

**Status:** fixed · **Reported:** 2026-08-24 (editing the capture draft's
headline showed a bare empty line where the row had stood) · **Surface:** the
sheet's title edit, over any headline whose title is empty · **Fixed in:**
`frontend/elm/src/Doc.elm`

## Symptom

On a headline with a title, the title edit is **tight**: the box stands in the
title cell's own slot with the star, the state, the priority and the tag run
drawn around it. On a headline whose title is **empty** the box covered the
whole line, and everything the row said vanished behind it — `* [#A]` became an
empty editable line.

The capture surfaced it, a fresh draft having no title by definition, but the
bug is the sheet's: any headline with an empty title wore it. Reproduced on a
real document by emptying a row's title through the commit door and pressing
`RET` on it — the box opened at x175 w1060 over a row at x174 w1060.

## Steps to reproduce

Serve `test/browser/tree`.

1. Filter to `priority:[#A]`, press `+`, type `bicycle`, press `RET`.
2. The head row reads `* [#A]`. Press `RET` on it. `#dtitle` opens the width
   of the whole row and `[#A]` is no longer on screen.

On a real document: capture `TODO placeholder` into the inbox, `POST
/headline` its org with the title cut to `* TODO`, open the sheet over it and
press `RET`. Same box, same swallowed row.

## Evidence

- `frontend/elm/src/Body.elm:1139` (`shown`) — a row draws the cells whose
  value is non-empty: `List.filter (\c -> c.val /= "") r.cells`. An empty title
  is therefore drawn as **no span at all**.
- `frontend/glue/20-sheet.js:518` (`dTitleAt`) — the title edit's anchor is
  `docElAt().querySelector(".dc-title") || docElAt()`, and its fallback is the
  whole line. With no title cell the fallback is what it always took.
- `placeEdit`'s `tight` arm (`:475`) then measures from that anchor's left edge
  to the `.dc-tags` edge or the row's right, which over the whole line is the
  whole line.

## Fix

The title cell is the **slot** the edit stands in, so the headline always draws
it — empty or not.

`drawnCells` (`Doc.elm:2067`) is `shown` with one exception: on a `Head` row the
title cell is kept whatever its value. `.dc-title` carries `flex: 1 1 auto`
already, so the empty cell grows into the space between the last decoration and
the tag run — which is exactly the slot a titled row's edit stands in. Every
other cell is still drawn only when it says something.

The anchor and the tight-box arithmetic are untouched: they were right, and had
nothing to anchor on.

Pinned in the browser rung, on both surfaces at once: *an empty title still has
a slot, and its edit stands tight in it* — the draft draws `* [#A] :bicycle:`
with the box at x236 w934 inside a 1060px row, every decoration clear of it,
and a real doc with an empty title draws `* TODO` with the box at x236 w999
clear of its state.
