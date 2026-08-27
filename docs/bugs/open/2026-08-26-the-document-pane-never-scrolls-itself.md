# Bug — the document pane never scrolls itself; `#mpanes` clips it instead

**Status:** open · **Reported:** 2026-08-26 · **Browser:** Chromium
· **Surface:** the materialize sheet's document pane

## Symptom

A subtree taller than the sheet is unreachable by mouse. `#mdoc` declares
`overflow:auto` and never draws a scrollbar, because its own height always
equals its content height; what clips the overflow is `#mpanes`, which wears
`overflow:hidden` and so offers the reader no scrolling mechanism at all. The
keyboard walk still reaches every row — `scrollIntoView` scrolls a hidden box
programmatically — so the pane looks correct under every key and is dead under
the wheel.

## Steps to reproduce

1. Open the sheet over a subtree with more lines than the sheet is tall (the
   driver tree's `drv-marks` in a 1000×380 window will do).
2. Scroll the pane with the wheel or a trackpad: nothing moves.
3. Read the geometry:

```js
const pane = document.getElementById("mdoc");
const panes = document.getElementById("mpanes");
({ mdoc: [pane.clientHeight, pane.scrollHeight],
   mpanes: [panes.clientHeight, panes.scrollHeight] })
```

At 1000×380 this answers `mdoc: [615, 615]`, `mpanes: [256, 615]` — the
document pane is as tall as its text and the row above it holds the 359px of
hidden content.

## Evidence

- `Page/Style.hs:115` — `#mpanes{flex:1;min-height:0;overflow:hidden;`
  `display:flex;flex-wrap:wrap;gap:10px}`.
- `Page/Style.hs:125` — `#mdoc{flex:2 1 320px;…;overflow:auto;…}`.
- `flex-wrap:wrap` is what does it. A flex line's cross size in a WRAPPING
  container is content-based; `align-content:stretch` may grow a line to fill
  the container but never shrinks one, so an item taller than the container
  makes the line taller than the container and overflows it. With
  `flex-wrap:nowrap` the single line takes the container's cross size and
  `#mdoc` would clip and scroll on its own.
- The wrap buys nothing today: the two panes are never both on screen —
  `Page/Style.hs:132`, `#sheet.raw #mdoc{display:none}` and
  `#sheet:not(.raw) #mtext{display:none}`.
- `docs/invariants.md` ("Shape") and `AGENTS.hs`'s pane note both say `#mdoc`
  owns its scroll. It does not; `#mpanes` does.

## What reads around it

`keepInView`/`placeRow` (`frontend/glue/20-sheet.js`) ask the ROW for its
nearest scroller rather than naming `#mdoc`, which is why the reading line and
`C-l` land correctly today. That is a reading of the layout, not a fix for it.

## The fix, when it is taken

Drop `flex-wrap:wrap` from `#mpanes` (or give `#mdoc` a `height:100%`). Both
the "the page never scrolls, sideways or down" and "no surface on the page
draws a scrollbar of its own" browser cases stand over that row and would have
to be re-read: `#mdoc` scrolling on its own means a scrollbar, which
`.tv-scroll`/`#kbd`'s own rule (`scrollbar-width:none` plus the webkit pseudo)
would have to cover.
