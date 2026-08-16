# Proposal — the box is measured rather than guessed

**Status:** proposed · **Date:** 2026-08-17 · **Origin:** raised directly while
sizing the `@` picker — hard-coded pixels are a portability risk, so what does
sizing this UI properly look like.

## The finding, in one line

`body{margin:0;font:14px/1.5 var(--glance-mono);…}`
(`src-web/Glance/Web/Page/Style.hs:51`)

The page pins its own base text size in device pixels, so a reader who has set
their browser's font size to 20px gets 14px anyway, and every one of the 143
other `px` figures in that file is a rung on a ladder nailed to the floor. The
same holds in the renderer (`font-size:15px` at `table-view.js:807`, `:841`,
`12px` at `:919`, `:973`, `:1099`, `11px` at `:1031`).

Everything else below follows from that one declaration.

## Three kinds of hard number, and only two are wrong

Sorting them matters, because converting the first kind would be vandalism.

**1. Device figures — leave them.** `1px` borders (21 sites in `Style.hs`),
`999px` pill radii, `0 4px 14px` shadows. A hairline is a hairline at any text
size; a border that grows with the font reads as a frame. `--tv-*-wash`
percentages are in the same class.

**2. Text sizes — the bug.** `font-size:12px`, `11px`, `10px`, `--g-doc-fs:13px`,
`--g-edit-fs:13px`, and the `14px` base they all hang off. These are the
reader's own setting being overridden. A reader who needs 20px text gets a
smaller page than their browser promised, and the deeper the surface the smaller
it gets: the picker's foot is `10px` (`Style.hs:290`), which is 71% of a base
that was already someone else's choice.

**3. Box sizes that hold text — wrong in a subtler way.** `width:min(1020px,…)`
on `#rbox`, `min(560px,100%)` on `.pop-band`, `320px`/`240px` panes,
`min-width:8em` on `.klab` (this one is right), and
`max-height:calc(12 * 2.05em)` on `.tv-inline .tv-scroll`. A box that holds N
rows of text should be N rows tall and M characters wide, whatever those measure
to. Written in px it holds fewer rows as the text grows — and it is the *reader
who needs bigger text* who loses the rows.

## The one that is a guess, and the fix that is already sitting there

`.tv-inline .tv-scroll{max-height:calc(12 * 2.05em)}`
(`../table-view/web/table-view.js`, the `INLINE` block)

`12` is the row count the picker wants. `2.05em` is a **guess** at the height of
a row whose box is actually owned by `.tv-table td`'s padding and line-height,
two rules away. Change either and the picker silently shows 11.6 rows.

The renderer already measures the real thing. `measure()` reads the drawn row
and header heights into `geom.row` / `geom.head` and redraws once when they
move (`table-view.js:2426-2429`); the whole windowing arithmetic is built on
`geom.row` (`:2392`, `:2450`, `:2773`). The cap is the only place that guesses
what `measure()` knows.

**Proposed:**

```js
// in measure(), beside `geom.row = h':
root.style.setProperty("--tv-row-h", `${geom.row}px`);
```
```css
.tv-inline .tv-scroll{ max-height:calc(var(--tv-inline-rows, 12) * var(--tv-row-h, 2.05em)); }
```

with `inlineRows` as a mount option writing `--tv-inline-rows`. The `2.05em`
survives as the fallback for the first paint, before a row exists to measure —
which is exactly the job `ROW_H = 30` already does for the spacers
(`table-view.js:605`). Two numbers become one measured value and one declared
row count, and the consumer owns the count instead of inheriting glance's `8`.

This is small, local, and testable today. It is the first step whatever else
happens.

## What "properly" looks like for the rest

### One base, and it is the reader's

```css
:root{ font-size:100%; }                    /* whatever the reader set */
body{ font:var(--g-fs-base)/1.5 var(--glance-mono); }
```
with `--g-fs-base:0.875rem` (today's 14px at a 16px default) and a scale beside
it — `--g-fs-sm:0.857em`, `--g-fs-xs:0.786em`, one step per existing size — so
`font-size:12px` becomes `font-size:var(--g-fs-sm)`. The rendered result is
byte-identical for a reader on defaults, and it *tracks* for everyone else.
Seven declared sizes replace the 40-odd literal ones, and the ladder is visible
in one block instead of spread over 300 lines.

### Boxes in `ch` and `em`, where they hold text

`#rbox{width:min(1020px, calc(100vw - 24px))}` becomes
`width:min(96ch, calc(100vw - 1.5rem))`. `ch` is the width of a `0` in the box's
own font, which is exactly the unit a monospace table is laid out in — the
column widths the renderer computes are character counts already. `.pop-band`'s
`560px` is `52ch`; `#plist`'s `40vh` is right as it stands.

### The host owns the width, the mount owns the rows

`inlineRows` above is half of it. The other half is that `#rbox`'s width is
glance's decision and the row count is the picker's, and today both are written
in the renderer or in glance by accident of which file was open.

### Container queries where the box is inside a pane

`--g-pop-max:min(90vh, calc(100vh - 2 * var(--g-pop-top)))` (`Style.hs:47-49`)
is a viewport measure on a box that lives inside `#modal`'s padding box. It is
right today because that box *is* the viewport minus padding, and it will stop
being right the first time a surface is embedded. `@container` is the honest
spelling once one is.

## The oracle, which is what makes it stick

This repo already knows that **figures are relational**
(`test/browser/cases.mjs:1-2`); every case computes a ratio rather than pinning
a pixel. The missing case is the one that changes the base:

> **the page at the reader's own text size, not the author's** — set
> `document.documentElement.style.fontSize` to 24px, and assert that the
> picker's box, the sheet's line box and the popup's foot all grew by the same
> factor as the root did.

Under today's stylesheet that case fails on every surface, which is the point:
it is the conversion's own progress bar, and it turns red the day someone types
a fresh `font-size:12px`. `drive.mjs` can drive it with no new machinery — it
already evaluates in-page and reads computed geometry, and the existing case 4
("the page never scrolls, sideways or down, at any width or surface") is the
same shape one axis over.

A second, cheaper oracle for the text half: a `TestServe.hs` case asserting no
`font-size:<digits>px` survives in the served stylesheet outside the token
block. That is a grep, it runs in 29 seconds with the rest, and it is the wall
that keeps the ladder in one place.

## Staging

1. **`--tv-row-h` + `inlineRows`** — the guess becomes a measurement. Sibling
   repo, ~10 lines, no glance change beyond passing the count.
2. **The token block and the base** — `Style.hs` only, mechanical, byte-identical
   at default settings.
3. **The relational browser case** — red at first, and it is the map.
4. **The `ch` conversions** for the four box widths, guided by 3.
5. **The renderer's own sizes**, same shape as 2, in the sibling repo.

## Risk

- **Steps 2 and 5 touch every surface at once.** The mitigation is that they are
  no-ops at a 16px default, which every existing browser case runs at — so the
  18 cases and the sibling's driver are the regression net, and the new case in
  step 3 is the only one that should move.
- **`ch` is font-dependent**, so a fallback face with different metrics changes a
  `96ch` box's pixel width. That is the correct behaviour (the box tracks the
  text it holds), but it does mean the picker is a different width when `Hack`
  is missing. Worth stating in the browser case rather than discovering.
- **The renderer is a shared library.** Its sizes are part of what a consumer
  sees; step 5 wants a line in the sibling's CHANGELOG, and the tokens want
  names a consumer can override, which is the actual win there.

## The scope of it

Every figure keeps the value it has today for a reader on defaults. What the
change buys is that the values are *derived* from one declaration instead of
typed 143 times, and that the one declaration is the reader's.
