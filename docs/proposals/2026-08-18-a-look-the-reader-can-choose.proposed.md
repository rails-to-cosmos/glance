# Proposal — a look the reader can choose

**Status:** proposed · **Date:** 2026-08-18 · **Origin:** raised while hiding the
doc pane's unordered bullets — the look is stored and honoured, and nothing yet
writes the key.

## The finding, in one line

The pane now has two looks and one of them is unreachable: `glance-bullets` is
read by the head script and spent by one CSS rule, and no control anywhere writes
it, so the choice can only be made from a console.

## What exists today

- `:root:not([data-bullets="shown"]) #mdoc .d-list .dbul{color:transparent}`
  (`Page/Style.hs`) — HIDDEN is the attribute's ABSENCE, so the default paints
  before any script runs, and only where a connector is drawn.
- The head script stamps `data-bullets` from `glance-bullets` when the stored
  value is the one the page knows (`Page/Style.hs`, `themeBoot`), the way it
  stamps `data-theme`.
- `bulletsKey` / `bulletsShown` (`Web/Theme.hs`, beside `themeIds`), mirrored in
  AGENTS.hs and joined by `TestSpec`.

## The shape of the missing half

The theme is the model, and it is five statements in `50-settings.js`:
`pref(key, def)`, a `setX` that stamps-or-deletes the attribute and stores the
value, that setter called once at boot, and a `change` listener on the control.
The bullets choice is the same five with the polarity inverted — `shown` stamps,
anything else deletes, which is what makes hidden the absence.

The control is one more `crow` inside the existing `#ctheme` part (`Page.hs`), so
`SECTIONS` needs no new part id and the settings boot cannot throw for a part it
has no markup for.

## Four traps, each already paid for once

- **The `<select>` ground.** `#themesel,#clayer,#nspace,#ngroup` name their
  background because a transparent control keeps the UA's paint — white on white
  in the native window (`6c56481`). A new `<select>` joins BOTH selector lists.
- **Which field opens the sheet.** `openSettings` focuses `#themesel` by name
  while `stepTab` takes the pane's FIRST control; the new row goes after the
  theme's, or both lines change together.
- **The harness models no nesting.** `field()` hangs every id flat off the body,
  so the control needs its own act (`bullets:`) and its own probe members the way
  `theme:` has them (`test/fixtures/shell-harness.js`).
- **The boot-time apply is load-bearing.** The node harness never runs the head
  script, so a stored preference is only observable because the glue re-applies
  it at boot — `setBullets(bulleted.get())` is what makes a keyed case possible.

## What it would be checked by

A sibling of `glue "the theme is a three-way switch the page honours"` in
`TestServe`, pinning the control's id, the `pref` line and the stamp/delete pair;
plus the keyed pair the theme already has — one driving the control and asserting
the attribute is stamped and remembered with no config write, one booting from a
store that already carries the choice.

## Sequencing

After the hidden bullets land, since the rule and the storage are what the panel
would drive. The panel is also where the next appearance choice goes, so it is
worth building the row generally: a list of `(label, key, values, default)` the
sheet walks, rather than a second hand-written select.
