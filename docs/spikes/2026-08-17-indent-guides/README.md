# Spike — eleven ways to say which block the cursor is in

**Date:** 2026-08-17 · **After:** `269ae41` “A rail per enclosing block, and the
one you are inside is lit”, taken back out in `98a381d`.

Navigating a nested list, nothing says where point is beyond the ground on its
own line — and less than before, since that ground now covers one line rather
than the whole subtree. The first attempt drew rails and shipped nothing worth
keeping: **it said how MANY blocks enclose point, never WHICH one.** Eleven looks,
built to be argued with. **Open `index.html`** — they are tabs.

Everything here is throwaway. The fixture is invented; the palette and the pane
are glance's own, lifted from `Theme/Default.hs` and `Page/Style.hs`, so the look
is judged at the real hues and the real metrics.

| file | what it draws |
| --- | --- |
| `index.html` | the tabbed shell; each variant runs in its own `<iframe>` |
| `a-rails.html` | a faint rail per enclosing block, none lit — depth as a COUNT |
| `b-active.html` | the same rails, exactly one lit: the block point is in |
| `c-bracket.html` | the lit rail starts at the line that OPENS the block and closes with a foot |
| `d-ladder.html` | nothing beside the text; a gutter rung per enclosing block by point's line |
| `e-ancestry.html` | every enclosing block lit, brightening inward |
| `f-path.html` | no marks at all — the ancestry spelled in words, riding the pane's top |
| `g-ancestry-path.html` | E and F on one ramp: each block's rail and its crumb carry the same strength |
| `h-no-ground.html` | G with no ground at all: every level marked by a rail, point's in the selection's hue |
| `i-hook.html` | H with every rail hung off the line that owns it, so a row you can descend into shows it |
| `j-brackets.html` | every mark a BRACKET: the line's own, the children it carries, the block it sits in |
| `k-tree.html` | the connectors `tree` uses — an elbow per row, a run where the branch goes on, a last child that closes it |
| `rig.js` | the fixture, the model, the keymap, the rail geometry |
| `pane.css` | the doc pane and both palettes |

**The rig is shared on purpose.** In the refer spike each rehearsal was a whole
page because they BEHAVED differently; here they differ only in the look, so one
fixture and one keymap is what makes two tabs comparable. A variant brings its
own `<style>` and one `paint` hook.

Keys are the pane's own: `n`/`p` walk siblings, `f`/`b` go in and out, and the
arrows alias both. `d` flags the row under point, `t` swaps the theme. The footer prints the truth — the depth
and the block — so a variant can be checked against what it is meant to say.

## Why the first attempt could not work

- **The accent rode a custom property, and a custom property inherits.** Lighting
  the block lit every row under it, so a row two deep looked exactly like the row
  one deep that owns it. A rail is a THING and has to be lit as one; each variant
  here paints rails individually and never through inheritance.
- **A rail per row is chopped up.** The rails lived in each row's
  `background-image`, so they restart at every row and break wherever a row has
  a margin or the cursor's ground takes the box. `b`–`e` and `g` draw an overlay
  layer instead: one element per rail, continuous, bounded to its block exactly.
- **The rail belongs at the tab stop LEFT of the block's children** — Sublime's
  rule, which is the parent's own column. A line at the outermost level has
  nothing to its left and draws nothing, which is why a paragraph is bare.

## The geometry, since it is not the box model

**The indent is in the TEXT.** `.d-item` carries `padding-left:0`, so every item
at every depth starts where the list's own text starts and its leading spaces do
the indenting. A guide therefore cannot ride a border: it is counted in `ch` from
the list's first line, two per level, org's own arithmetic.

`Html.Attributes.style` in Elm 0.19 assigns `style[key]` and browsers ignore that
for `--x`, so anything a variant needs per row travels as a class or an attribute
— `data-depth` here.

Any ground sharing a box with a rail must be `background-color`; the
`background` shorthand resets `background-image` and the rails vanish. This one
already cost a debugging session.

## The complaint, mechanised

`depth-check.mjs` walks each variant to four depths and counts how many DISTINCT
pictures it draws, reading the guide alone and never the cursor's ground — the
ground moves on every step and would make a blind variant look like a seeing one.

```
flat a-rails.html         1/4 distinct
ok   b-active.html        4/4 distinct
ok   c-bracket.html       4/4 distinct
ok   d-ladder.html        4/4 distinct
ok   e-ancestry.html      4/4 distinct
ok   f-path.html          4/4 distinct
ok   g-ancestry-path.html 4/4 distinct
ok   h-no-ground.html     4/4 distinct
ok   i-hook.html          4/4 distinct
ok   j-brackets.html      4/4 distinct
ok   k-tree.html          4/4 distinct
```

`a-rails.html` is the baseline and it is FLAT by construction: the picture is the
same wherever point is, which is what the reverted work amounted to once the
inherited accent is discounted. It stays in the spike as the control.

```sh
node depth-check.mjs                 # every variant
node depth-check.mjs b-active.html   # one
node shell-check.mjs                 # the tabbed shell mounts all eleven
node place-check.mjs                 # every mark is where it says it is
```

Firefox over WebDriver BiDi, no dependencies — `bidi.mjs` is the refer spike's
driver, copied so this directory stands alone.

## I, which also says what you can go into

`f` descends and does nothing on a leaf, and until now nothing on the screen said
which was which before the key was pressed. **I hangs every rail off the line that
OWNS it** rather than off its first child. A row with something inside is then the
only kind of row a rail leaves; a leaf is a line with nothing under it. It costs no
ink that was not already drawn — the same rails, starting one row higher.

Two signals then agree, which is why this is a change to H and not a new language:
point's own mark spans its whole extent, so a leaf's mark is exactly one line tall
and a container's covers its subtree.

What it cannot say: an EMPTY container looks like a leaf. Org writes no such row
here, so it costs nothing in this fixture and would cost something in a tree that
has them.

## K, which is the one to build

A rail says how deep. A BRACKET says where a block starts and stops. The
connectors `tree` draws say both, plus the one thing neither can: **which child is
the last**, because its branch closes rather than carrying on. K spends no weight
axis at all — the elbow shape carries the structure and the ink carries the state.

Three sentences, each answering a different question:

- **Point** is gold: its own elbow, and the run below it where its branch goes on.
- **Its ANCESTORS** are blue on the ramp, brightening inward — the ancestors
  themselves and not their siblings. Lighting every sibling of every ancestor lit
  three whole levels and said nothing about the way back.
- **A composite at point** lights the ROOTS it opens and stops there. A list has no
  connector of its own, and lighting every descendant painted the pane gold and
  named no stop in particular.

An ITEM lights itself alone, which is the rule the cursor's ground was given: an
item is its own line, not what hangs off it. **What point CARRIES takes the same
gold a shade back** — J's tier, which says the subtree belongs to the stop without
competing with the line that IS it.

**Org's bullet is the branch's tip.** The elbow turns and stops: its horizontal
sat at the same height as the file's own `-`, one cell to the left, and the two
read as one dashed run rather than as a branch. `STUB` is the knob and it is `0`;
`0.45` of a character puts the horizontal back. A paragraph is not a branch — nothing
to elbow into — so it keeps the vertical bar, at the column every outermost row
marks on.

**The base stroke is auxiliary.** Every connector is a hairline until it is lit;
a mark that weighs as much as the text competes with it.

`J` is the same idea with brackets instead of elbows and stays for the comparison:
it says where a block ends, which K says by closing the branch, and it costs a cap
at both ends of every mark. `H` shipped first (`795d1da`) and is what K replaces.

## Two placement traps, recorded because each looked like a page bug

- **A strip written into the pane's flow moves everything below it.** Filling it
  after the rails are measured puts every mark one line high, and the page looks
  right again the moment a key is pressed — so it survives every check that
  presses a key first. `place-check.mjs` asserts a repaint that changes nothing
  changes nothing; reordering the two writes is the fix.
- **A throw in the rig's own status line takes the paint with it.** One undefined
  name in the footer's readout and no rail was drawn at any list stop, while the
  paragraph stops — which took the other branch — drew fine.

`place-check.mjs` holds five: SETTLED (a repaint changes nothing), COLUMN (one x
per level, whatever the element), OPAQUE (no rail composites with another), HEAD
(bold over a row's own text, thin over its subtree, and a headline's stars
instead of a bar), and RAMP (a block's
rail and its crumb carry the same strength — G shipped an hour with the two
counted from different ends, every block a step apart).

## What each costs

| | says WHICH block | says HOW DEEP | ink added | needs a paint hook |
| --- | --- | --- | --- | --- |
| A rails | no | by counting | rails everywhere | no — pure CSS |
| B active | yes | by which rail | rails everywhere, one accent | yes |
| C bracket | yes, with its extent | by which rail | as B, plus two caps | yes |
| D ladder | no | yes, read directly | a gutter, one row's worth | yes |
| E ancestry | yes, whole chain | by how many are lit | rails everywhere, N accents | yes |
| F path | yes, by NAME | by crumb count | a strip, nothing beside the text | yes |
| G both | yes, drawn AND named | both, and they agree | as E, plus F's strip | yes |
| H no ground | yes, drawn AND named | both, at one column per level | as G, plus a gutter rail; NO ground | yes |
| I hooks | as H, and which rows have something inside | as H | as H, no more | yes |
| J brackets | yes, with its ends | by which bracket | three hairline brackets | yes |
| K tree | yes, and the way back to it | by the branch | an elbow per row | yes |

## G, and the ramp it settled

E and F are the same chain said twice, so **G gives them one ramp**: a block's
rail and its crumb carry the same strength, full at point and a third shed per
step out. The strip is then a legend for the rails and the rails are a picture of
the strip, and neither has to be read on its own — the eye that starts at the
words ends at the line, and the eye that starts at the line ends at the words.

G draws no rail for the outermost block, so its first crumb names something the
picture does not show; `H` is what closes that gap.

Two details it settles, and both could go the other way:

- **A crumb mixes toward the muted ink, never toward nothing.** A crumb at a
  fifth of the accent over transparent is a crumb nobody can read; a rail at a
  fifth over transparent is exactly right. Text and mark take the same ramp and
  different grounds.
- **The last crumb is point itself**, bold and at full accent, which is also
  where the innermost rail sits — so the two brightest things on the screen are
  the line you are on and the block that holds it. Naming only the ENCLOSING
  blocks and stopping short of point is the alternative: tighter as a legend,
  and it drops the one crumb that says which item.

## H, which took the ground away and shipped

The cursor's golden band is gone and the cursor is a vertical mark like
everything else — the same width as every rail, separated from them only by its
hue, which is the selection's own gold. Three things follow, and each is a rule
the earlier variants did not need:

- **One column per level, whatever the element.** A mark sits one tab stop LEFT
  of its row's own text, which is where its block's rail already runs — so point
  rides its block's rail and its own stretch of it turns gold. A paragraph, the
  headline, the list and a top-level item are all at the outermost level and mark
  at one x.
- **The document is a block too**, and its rail runs the whole pane. Without it
  the outermost column is drawn beside the list and nowhere else, so it breaks at
  every paragraph.
- **The point mark spans the row's whole extent**, own line and subtree together,
  which for a leaf is one line and for the list is the list.
- **One gold, two weights.** The row's own text takes the BOLD stroke and what
  hangs off it takes the THIN one, both solid — dimming gold toward a dark ground
  darkens it, and a darker yellow is brown. A paragraph carries nothing and is bold
  over every line it wraps to, and so is the list, which IS the stop.
- **A flag outranks the cursor.** `d` flags the row under point and its mark turns
  red — the same geometry, `--g-bad` instead of the gold, so a flag takes the
  mark's place rather than sitting beside it. It is drawn wherever it is, not only
  under point, so a flagged row you are not on still shows red; point draws last
  and so wins the overlap. A row that is both wears the flag: the flag is the
  louder fact.
- **A row wears the marker org already wrote.** A headline's stars sit in the
  column a bar would use, so a selected headline turns its stars gold and draws no
  bar at all; for the same reason the document's rail starts UNDER the headline.
  An item's bullet sits clear of the bar, so it goes gold as well as one — and the
  bullet is whatever org wrote, `-` or `+` or `*` or `1.`, never a hyphen the rule
  knows in advance.
- **Every rail is one width and every rail is opaque.** A wider mark reads as a
  different KIND of thing; and since the document's rail and the list's run the
  same column, a translucent rail composites darker along the list than beside a
  paragraph — the same unhighlighted mark in two styles.

What it costs: the ground was the one mark that could not be missed, and a 2px
rail in the gutter is a smaller claim on the eye than a golden band. Whether that
is restraint or a loss is what the tab is for.

The others stay for the record. `B` is Sublime's own answer and the smallest
thing that works; `C` adds where the block ENDS; `D` is the cheapest, and the
only one that draws nothing among the words; `A` is the control.

**G is drawn from JS and glance's doc pane is Elm.** The shipping form is an
overlay Elm draws from the rows it already has — it knows every row's depth and
owner, which is the whole of the geometry — plus the strip, which is the chain it
already walks for `b`. `Html.Attributes.style` cannot set `--near`, so the ramp
travels as a class per step (`up-0` … `up-3`) or as an inline `background` the
paint computes.
