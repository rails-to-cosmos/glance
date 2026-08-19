# Spike — five ways to say a block folds, and that it did

**Date:** 2026-08-19 · **After:** the headline-bars spike, whose F (the ramp)
shipped; the fold's only mark today is an ellipsis riding the folded title.

TAB folds a block and " …" says it happened — after the fact, in words, and
never before it: nothing on the screen says a headline CAN fold until it is
tried. Five looks, built to be argued with. **Open `index.html`** — they are
tabs.

Everything here is throwaway. The rig is the headline-bars spike's with the
shipped spine ramp BUILT IN as the stage: what differs between tabs is the
fold mark alone. A variant brings a `foldText` hook (what rides the folded
title) and, if it draws, one `paint` over the `#marks` layer.

| file | what it draws |
| --- | --- |
| `index.html` | the tabbed shell; each variant runs in its own `<iframe>` |
| `a-ellipsis.html` | the control: " …" riding the folded title, the shipped look |
| `b-signs.html` | +/− chips on the spine's column at every foldable headline |
| `c-chevron.html` | org-modern's ▾/▸ at the same column, no box |
| `d-count.html` | the folded title carries " +N" — the rows TAB will bring back |
| `e-stub.html` | a folded block keeps a short, thick stub of its own spine |
| `rig.js` | fixture, model, keymap, fold, and the shipped ramp as the stage |
| `pane.css` | the doc pane, both palettes, the spine base |
| `check.mjs` | the complaint, mechanised |
| `bidi.mjs` | the Firefox driver, copied so this directory stands alone |

Keys are the pane's own: `n`/`p`, `f`/`b`, arrows, `TAB` folds, `d` flags,
`t` swaps the theme.

## The axis the tabs argue over

Two different questions hide in one mark:

- **Whether** — can this headline fold; is it folded now? B and C answer both,
  and only they speak BEFORE the fold. Their cost is permanent ink: a mark per
  headline, always, in a pane that otherwise draws chrome only where point is.
- **What happened** — something is hidden here. A, D and E answer only that.
  A says it in org's own words; D upgrades the words to a fact (" +N" rows);
  E says it in the spine's own language — the bar compressed to a stub — and
  spends no words at all.

Learned on the way: at the rail's column there is no free air — B's chip and
C's glyph land where the STAR already sits, so they read as replacing the
marker rather than riding beside it. That is org-modern's own move, but it is
a bigger claim than "an icon on the spine": the star is content.

## The check

```sh
node check.mjs                  # every variant
node check.mjs e-stub.html      # one
```

Per variant: FOLD (TAB hides rows, changes the picture — spines, marks and
fold-texts together — and a second TAB restores it byte for byte), SETTLED
(a repaint that changes nothing changes nothing).

## What each costs

| | before the fold | after the fold | ink | words |
| --- | --- | --- | --- | --- |
| A ellipsis | nothing | " …" | none | one glyph |
| B signs | a chip per headline | the chip flips to + | a box, always | one sign |
| C chevron | a glyph per headline | it turns | a glyph, always | none |
| D count | nothing | " +N" | none | a number |
| E stub | nothing | a stub of the spine | a stub, folded only | none |

**E is the one to build** if the pane keeps its text-and-hairlines character:
it answers in the medium the pane already speaks, appears only when true, and
composes with D (" +N" beside the stub) if the count earns its keep. B/C are
the answer if "what can I fold?" turns out to matter more than quiet — that is
a question about users, and the tabs exist so it can be asked at the screen.
