# Spike — six ways to say which block the cursor is in

**Date:** 2026-08-17 · **After:** `269ae41` “A rail per enclosing block, and the
one you are inside is lit”, taken back out in `98a381d`.

Navigating a nested list, nothing says where point is beyond the ground on its
own line — and less than before, since that ground now covers one line rather
than the whole subtree. The first attempt drew rails and shipped nothing worth
keeping: **it said how MANY blocks enclose point, never WHICH one.** Six looks,
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
| `d-ladder.html` | nothing in the prose; a gutter rung per enclosing block beside point's line |
| `e-ancestry.html` | every enclosing block lit, brightening inward |
| `f-path.html` | no marks at all — the ancestry spelled in words, riding the pane's top |
| `rig.js` | the fixture, the model, the keymap, the rail geometry |
| `pane.css` | the doc pane and both palettes |

**The rig is shared on purpose.** In the refer spike each rehearsal was a whole
page because they BEHAVED differently; here they differ only in the look, so one
fixture and one keymap is what makes two tabs comparable. A variant brings its
own `<style>` and one `paint` hook.

Keys are the pane's own: `n`/`p` walk siblings, `f`/`b` go in and out, and the
arrows alias both. `t` swaps the theme. The footer prints the truth — the depth
and the block — so a variant can be checked against what it is meant to say.

## Why the first attempt could not work

- **The accent rode a custom property, and a custom property inherits.** Lighting
  the block lit every row under it, so a row two deep looked exactly like the row
  one deep that owns it. A rail is a THING and has to be lit as one; each variant
  here paints rails individually and never through inheritance.
- **A rail per row is chopped up.** The rails lived in each row's
  `background-image`, so they restart at every row and break wherever a row has
  a margin or the cursor's ground takes the box. `b`–`e` draw an overlay layer
  instead: one element per rail, continuous, and bounded to its block exactly.
- **The rail belongs at the tab stop LEFT of the block's children** — Sublime's
  rule, which is the parent's own column. A line at the outermost level has
  nothing to its left and draws nothing, which is why prose is bare.

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
flat a-rails.html    1/4 distinct
ok   b-active.html   4/4 distinct
ok   c-bracket.html  4/4 distinct
ok   d-ladder.html   4/4 distinct
ok   e-ancestry.html 4/4 distinct
ok   f-path.html     4/4 distinct
```

`a-rails.html` is the baseline and it is FLAT by construction: the picture is the
same wherever point is, which is what the reverted work amounted to once the
inherited accent is discounted. It stays in the spike as the control.

```sh
node depth-check.mjs                 # every variant
node depth-check.mjs b-active.html   # one
node shell-check.mjs                 # the tabbed shell mounts all six
```

Firefox over WebDriver BiDi, no dependencies — `bidi.mjs` is the refer spike's
driver, copied so this directory stands alone.

## What each costs

| | says WHICH block | says HOW DEEP | ink added | needs a paint hook |
| --- | --- | --- | --- | --- |
| A rails | no | by counting | rails everywhere | no — pure CSS |
| B active | yes | by which rail | rails everywhere, one accent | yes |
| C bracket | yes, with its extent | by which rail | as B, plus two caps | yes |
| D ladder | no | yes, read directly | a gutter, one row's worth | yes |
| E ancestry | yes, whole chain | by how many are lit | rails everywhere, N accents | yes |
| F path | yes, by NAME | by crumb count | a strip, no marks in prose | yes |

**B is the recommendation.** It is the look that was asked for, it answers the
question the first attempt got wrong, and it adds one accent line to a pane that
otherwise gains nothing. `C` is B plus the opening line, worth trying at the
keyboard before choosing between them: the foot tells you where the block ENDS,
which is the second question after where you are.

`F` is orthogonal to the rest and could ship beside any of them — it is the only
one that answers WHERE rather than HOW DEEP, and it costs no marks in the prose.
`D` is the cheapest thing that would satisfy the original complaint on its own.
`E` lights more than it needs to: the whole ancestry is legible, but the innermost
rail — the only one that answers the question — has to be picked out of a ramp.

**`b`–`e` are drawn from JS and glance's doc pane is Elm.** Whatever wins, the
shipping form is either an overlay Elm draws from the rows it already has, or —
for `B` alone — pure CSS: `.d-item:has(> .dat)` is the block point is in, and it
can light its own rail without anything measuring anything. That is the first
thing to try, and it is untested here.
