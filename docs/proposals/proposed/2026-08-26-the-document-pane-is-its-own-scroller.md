# Proposal — the document pane is its own scroller

**Status:** proposed · **Date:** 2026-08-26 · **Origin:** user — *"it should
be reachable for wheel, trackpad or touch scrolling"* — over
`docs/bugs/open/2026-08-26-the-document-pane-never-scrolls-itself.md`.

## The law in one line

`#mdoc` clips and scrolls its own document — the wheel, the trackpad and a
finger move it; the keyboard walk, the reading line and `C-l` keep asking
the row's own scroller, which is now the pane the invariants already name.

## What is wrong, measured

`#mpanes{overflow:hidden;display:flex;flex-wrap:wrap}` (`Page/Style.hs:115`)
and `#mdoc{flex:2 1 320px;…;overflow:auto}` (`:125`). A flex line in a
WRAPPING container takes its content's cross size — `align-content:stretch`
grows a line but never shrinks one — so `#mdoc` is always exactly as tall as
its text (`scrollHeight === clientHeight`, never a bar), and the row above
it, `#mpanes`, holds the overflow behind `overflow:hidden`. At 1000×380 over
`drv-marks`: `mdoc [615, 615]`, `mpanes [256, 615]`. The keyboard reaches
every row because `scrollIntoView` moves that hidden box; the pointer
reaches nothing. The wrap buys nothing: the two panes are never both on
screen (`#sheet.raw #mdoc{display:none}`, `#sheet:not(.raw) #mtext{display:none}`,
`:132-133`). `docs/invariants.md` ("Shape") and `AGENTS.hs:5323` say `#mdoc`
owns its scroll — true only after this change.

## The change

```
#mpanes{flex-wrap:nowrap}                 /* the single line takes the container's height */
#mdoc{overscroll-behavior:contain;        /* a pane at its end never scrolls the page */
      touch-action:pan-y;                 /* a finger pans the document, the page keeps the rest */
      scrollbar-width:none}
#mdoc::-webkit-scrollbar{width:0;height:0}
```

- **Wheel and trackpad** are native once the overflow is real; nothing in
  the glue changes. `el("mdoc").addEventListener("scroll", placeEdit, true)`
  already listens on this box, so the edit overlay follows.
- **Touch**: `pan-y` grants vertical panning to the pane and leaves
  horizontal and pinch to the page; `overscroll-behavior:contain` keeps the
  pinned law "the page never scrolls, sideways or down".
- **The scrollbar is hidden** (decided, user, 2026-08-26): the sheet's
  standing rule ("no surface on the page draws a scrollbar of its own")
  extends to `#mdoc` the way `.tv-scroll` and `#kbd` already spell it. The
  reading line and `C-l` are the position cues.
- **Point never moves under the pointer.** Scrolling by wheel or finger
  changes what is on screen and nothing else; the next key acts at point,
  and `keepInView` (the reading line's band, the 3-line margin) brings
  point's row back on screen — Emacs's own: point stays, the window moves,
  the next command re-scrolls to point.
- `keepInView`/`placeRow`/`askScroller` are untouched: they already ask
  the row's nearest scroller, which becomes `#mdoc`.

## Docs

`docs/invariants.md` "Shape" and the `AGENTS.hs` pane note become true as
written; add the pointer law beside them: `#mdoc` is the document's ONE
scroller, the pointer moves it, a key moves point. The bug file closes.

## Oracle

Browser, over a subtree taller than the sheet:
- `#mdoc.scrollHeight > #mdoc.clientHeight` and
  `#mpanes.scrollHeight === #mpanes.clientHeight` (the overflow lives in
  the pane);
- a `wheel` event (or `scrollTop` set) moves the content and leaves point
  and `docAtNow()` unchanged; `n` afterwards rests the row on the reading
  line;
- `getComputedStyle(#mdoc).touchAction === "pan-y"`;
- the two standing cases re-read and still pass: the page never scrolls
  (`overscroll-behavior`), no surface draws a bar (`#mdoc`'s is hidden).
TestServe: the four CSS declarations as glue needles beside the `.tv-scroll`
rule's.

## LOC

Five CSS lines; two doc entries; one browser case; two cases re-read.

Inert until reviewed.
