# Proposal — hue is never the only channel

**Status:** proposed · **Date:** 2026-08-17 · **Origin:** raised directly while
choosing the doc pane's indent guides — a theme for colour-blind readers, and
what a theme can and cannot fix.

## The finding, in one line

A theme changes hues. Where a distinction rides hue ALONE, no palette can rescue
it — so this is two pieces of work, and only the first is a theme.

## What glance already gets right, and it is most of it

The audit's happier half. Every one of these carries its meaning in TEXT and uses
hue as reinforcement:

- **A state badge draws its word** — `TODO`, `NEXT`, `DONE`. The hue families
  (`pActive` warm, `pInactive` cool, `Theme/Default.hs:36-37`) say the same thing
  a second way.
- **A priority badge draws its letter**, `A`/`B`/`C`. `pPriority` is
  `[red, amber, green]` (`Theme/Default.hs:38`) — the textbook deuteranopia pair —
  and it is redundant, because the letter is already there.
- **The log's severity and scope are columns of text.**
- **The doc pane's structure is now SHAPE**: a connector down to the middle of the
  line, a run where the branch goes on, and a branch that closes on the last child
  (`Page/Style.hs`, the `.d-list .d-item::before` block). Depth, ancestry and
  "what is inside this" survive greyscale.

The rule that produced them, stated so the next surface inherits it:

> **HUE IS NEVER THE ONLY CHANNEL.** Every distinction the reader must make is
> carried by text, shape, or position as well. Hue makes it faster, never
> possible.

## Where the rule is broken today

Three places, each hue-only.

**1. The doc pane's flag against its cursor.** Both are a hairline connector in
the row's own column; they differ by hue alone — `--g-bad` red against
`--g-point` gold (`Page/Style.hs`). Under protanopia the red darkens toward the
gold's neighbourhood and a flagged row reads as the cursor. This one arrived with
the tree, so it is the newest and the cheapest to fix.

**2. The table's flagged row against its selected row.** Two grounds,
`--tv-flag`-washed and `--tv-sel`, distinguished by hue and nothing else.

**3. `--g-ok` / `--g-warn` / `--g-bad` as bare dots or washes** wherever they
appear without their word beside them.

## The half that is a theme

Two more entries in `themes` (`Theme.hs:30-33`), a light and a dark, built on the
**Okabe–Ito** set — the eight-colour palette designed to stay distinct under
protanopia, deuteranopia and tritanopia:

| role | today (light) | proposed |
| --- | --- | --- |
| `pBad` | `#E74C3C` | `#D55E00` vermilion |
| `pWarn` | `#FFA500` | `#E69F00` orange |
| `pOk` | `#27AE60` | `#0072B2` blue |
| `pAccent` | `#31769F` | `#0072B2` blue |
| `pPriority` | red / amber / green | `#D55E00` / `#E69F00` / `#0072B2` |
| `pActive` | four warms | vermilion, orange, reddish-purple, brown |
| `pInactive` | four cools | blue, sky, bluish-green, grey |

The active/inactive families stay **warm against cool**, which is the one
opposition that survives every common deficiency — what changes is that no pair
inside a family is a red/green discrimination.

What it costs, all of it mechanical:

- `Palette` gains no field; two records are added to `Theme/Default.hs` and two
  entries to `themes`.
- `themeIds` (`Theme.hs:38`) grows, so the boot script's `data-theme` test and the
  settings sheet's theme list follow it — both already read that list.
- `TestSpec`'s token sweep asserts every role a theme declares, so a new theme is
  checked by construction (`TestSpec.hs:1122-1126`).
- Browser case 21 asserts each theme declares its `color-scheme`; a new theme
  joins that case rather than needing its own.

**Naming.** `cud-light` / `cud-dark`, after the Color Universal Design set they
come from, so the id says where the numbers are from.

## The half that is not a theme

For the three breaks above, a palette is the wrong tool. Each needs a second
channel, and each is a small drawing change:

- **The flag's connector takes a shape of its own** — drawn DOUBLE (two hairlines
  a pixel apart) or dashed, so a flagged row differs from the cursor with the hue
  discarded. The spike's `STUB` knob shows the machinery is already there
  (`spikes/2026-08-17-indent-guides/k-tree.html`).
- **The table's flagged row keeps its inset edge** — it already draws one
  (`groundSweep` asserts `box-shadow:inset 3px 0 0 var(--g-bad)`), so the fix is
  to make the EDGE the primary signal and the wash its reinforcement.
- **A severity dot never appears without its word**, which is a review of the
  sites rather than a change to the palette.

## How it would be checked

The palette half is checked by the sweep that already exists. The second half
wants a check the repo does not have yet: **a case that reads the page with hue
discarded**. Concretely — render the sheet, take each pair that must stay
distinct (cursor against flag, selected row against flagged row), convert both
inks to their relative luminance, and assert the pair differs by more than a
threshold. That is a browser case in the existing harness, and it fails today for
the flag against the cursor, which is the point.

## Sequencing

1. The **luminance case** first, red, naming the three breaks.
2. The **flag's shape**, which turns it green for the doc pane.
3. The **two themes**, which are mechanical once the sweep is in.
4. The table's edge, and the severity-dot review.

Doing the theme first would produce a palette that is safe on paper and a pane
where a flag still reads as a cursor for the reader it was built for.
