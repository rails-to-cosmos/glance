# Spike — six ways to mark a nested headline's block

**Date:** 2026-08-19 · **After:** the child-shelf work (`a74685c`…), which put
every descendant's contents in the pane and left the bars as they were: one per
row, none per block.

A nested headline owns a run of rows, and nothing on the screen says which run.
The shipped bars light the PATH — point's owners, flat — so standing on the
root and standing on a child paint the same picture wherever both light
everything. Six looks, built to be argued with. **Open `index.html`** — they
are tabs.

Everything here is throwaway. The fixture is invented; the palette and the pane
are glance's own, lifted from `Theme/Default.hs` and `Page/Style.hs`, so the
look is judged at the real hues and the real metrics.

| file | what it draws |
| --- | --- |
| `index.html` | the tabbed shell; each variant runs in its own `<iframe>` |
| `a-shipped.html` | the control: a bar per row at its shelf's rail, exactly what ships |
| `b-tree.html` | the list's elbows lifted to the shelf: a branch per row, hung off the headline |
| `c-spine.html` | one unbroken bar per block, first row to last |
| `d-perforated.html` | C dotted at rest, solid where point is — texture before hue |
| `e-bracket.html` | C hung from the headline's own line and closed with a foot |
| `f-ramp.html` | C with the ancestry lit, brightening inward |
| `rig.js` | the fixture, the model, the keymap, the fold, the shelf geometry |
| `pane.css` | the doc pane and both palettes |
| `check.mjs` | the complaint, mechanised |
| `bidi.mjs` | the indent-guides spike's Firefox driver, copied so this directory stands alone |

Keys are the pane's own: `n`/`p` walk siblings, `f`/`b` go in and out, the
arrows alias both, `TAB` folds a headline's subtree, `d` flags the row,
`t` swaps the theme. The footer prints the truth — the shelf and the block —
so a variant can be checked against what it is meant to say.

## The geometry, and where it crosses

The shipped columns, in characters of the pane's text edge, for a row at
shelf `d`: the star at `2d`, the contents at `d+2`, the rail at `d+0.5`.
The stars step TWO a shelf and the contents step ONE, so the two meet:

- shelf 2 — the contents start AT the star's column;
- shelf 3 — the contents start LEFT of their own star, which the
  great-grandchild in the fixture shows on every tab.

Two consequences the spike had to learn:

- **A mark "under the star" is a mark through the text** from shelf 2 on.
  Every block variant started there and moved to the RAIL — `d + 0.5`, the
  column the shipped per-row bars already stand in and the only one that is
  clear at every shelf. The deeper fix is the shelf step itself, and it is out
  of scope here.
- **The drawer's hanging colon lives in the gutter too**, so B's elbow brushes
  it at the root shelf. A mark column and a punctuation overhang contend for
  the same air.

## What the control cannot say

`check.mjs` walks out of the grandchild's paragraph and asks for a distinct
picture at each headline stop, reading the marks alone and never the cursor's
ground. The control fails by construction — at the root and at the child every
bar is lit the same, because the shipped tiers light the path and the path
covers everything from both stops. It stays in the spike as `flat`, the
baseline the other five are measured against.

One relaxation, on purpose: being ON a headline and being INSIDE it paint the
same block-marks — the chain holds the same blocks either way — and the ground
is what says which. The check therefore requires the three headline stops
distinct, plus in-vs-out one shelf apart.

```sh
node check.mjs                 # every variant
node check.mjs c-spine.html    # one
```

Per variant: WALK (each shelf its own picture), FOLD (`TAB` hides rows,
changes the picture, and a second `TAB` restores it byte for byte), SETTLED
(a repaint that changes nothing changes nothing).

## What each costs

| | says WHICH block | says its EXTENT | ink added | needs a paint hook |
| --- | --- | --- | --- | --- |
| A shipped | no | no | a bar per row | no — pure CSS |
| B tree | yes, and the way back | yes, the branch closes | an elbow per row | yes |
| C spine | yes | yes, by its ends | one bar per block | yes |
| D perforated | yes, solid vs dotted | as C | as C | yes |
| E bracket | yes | yes, capped both ends | as C plus a foot per block | yes |
| F ramp | yes, whole chain ranked | as C | as C, N accents | yes |

## B, which unifies the grammar

The pane already draws a list as branches; B says a subtree of headlines is
the same thing one storey up, and one grammar reads at both nestings. Its ink
follows the list spike's own K law — point, what it CARRIES, and its
ancestors' own connectors, never every sibling — because the class-based tiers
the shipped rows wear went flat here exactly as the control does. It is also
the busiest tab: every row pays an elbow whether or not the structure is in
question.

## C, D, E — the block as one thing

C is the smallest mark that answers the question: one bar, one block, gone
when the block folds. D spends TEXTURE where the others spend hue — dotted at
rest, solid under point — so the answer survives a colorblind reader and a bad
monitor. E adds the one fact C leaves out: where the block ENDS, said with a
foot, which is also precisely what `TAB` is about to take away.

**C is the one to build, with D as its stance on accessibility** — the same
geometry, so the choice between them is a stroke style, not an architecture.
E's foot is worth having only if the fold turns out to need the affordance;
F's ramp says WHICH ancestor at the cost the shipped pane already refused
once ("dimming the rest is what makes the path read" — the flat tiers).

## What the rig mirrors, so the tabs are honest

The rows are FLAT in the DOM, as in the shipped pane — nesting is in the
model, ownership settles what a fold hides, and the tiers (`up`, `sib`, what
point carries) are classes the rig refreshes per step, since the shipped
selectors that read DOM nesting reach nothing here. The child headline wears
the headline's own face (`.d-head`'s weight), the drawer folds shut by
default, and the fixture carries a list inside a child so a block mark and the
list's elbows are judged together.
