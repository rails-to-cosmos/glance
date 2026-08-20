# Bug — the cursor grounds a list item's whole subtree

**Status:** fixed · **Reported:** 2026-08-17 · **Surface:** the materialize
sheet's document pane · **Fixed in:** `src-web/Glance/Web/Page/Style.hs`

## Symptom

Navigating a list in the material document, the cursor's ground covers the item
**and everything nested under it**, rather than the item's own line.

## Steps to reproduce

Open the sheet over an entry whose body holds a list with a nested item, walk
to the list (`n`), and go finer into it (`f`). The item under the cursor paints
its ground down the whole nested block.

Measured on the driver's own fixture: the item's own line is **21px**, the
element it is drawn as is **83px**, and all 83 were grounded.

## The cause

A nested item is drawn INSIDE its parent — `Doc.elm`'s `viewKids` puts the
parent's own text and its children in one `div.de` — so the parent's element is
as tall as its subtree. The cursor rule grounded that element:

```css
#mdoc.on .de.dat{background:var(--g-sel);color:var(--g-fg)}
```

The nested rows carry no ground of their own, so the parent's showed through
every one of them.

## The fix

The rows nested under the cursor take the pane's own ground back:

```css
#mdoc.on .de.dat .de:not(.dfl){background:var(--g-bg)}
```

`:not(.dfl)` leaves a flagged child its own wash, which is a higher claim on
the one background slot than the cursor is.

## What catches it going wrong

`test/browser/cases.mjs` — "the cursor on a list item grounds the item, not the
subtree under it". It walks with `n` and goes finer with `f` (a list is ONE stop
at the coarse grain), then asserts the nested row's ground is **opaque** and
differs from the cursor's. Opacity is the load-bearing half: a transparent
nested row reads as a different colour while still letting the ground through,
which is the bug wearing another spelling. `BREAK=subtree-ground` spells exactly
that and turns the case red; so does removing the rule.

## Noted in passing, not fixed

The pane's composite case (`a composite's drawn lines sit on the same grid as
the field over it`) fails on a nested list from the other side: a leaf with
children stands three line boxes tall where the case expects one, and reports
"every leaf under it is pushed down". Whether the pane should draw a nested item
as a sibling row rather than inside its parent is a design question this fix does
not answer — it only stops the cursor claiming the subtree. The fixture that
provokes it lives in `test/browser/tree/wide.org`, on the one entry no case
writes to.
