# Spike — three rehearsals of `@` in the materialize sheet

> **Superseded in one detail, 2026-08-17.** The prefixed `@` below is
> spelled `C-u @` throughout, org's universal argument. The shipped keymap
> cannot use it — `C-u` is the browser's view-source and is now a reserved
> chord — so the live design spells it `C-c @`. The rehearsal is left as it
> ran.

**Date:** 2026-08-16 · **For:**
[`../../proposals/2026-08-15-a-relation-is-a-link-with-a-kind.proposed.md`](../../proposals/2026-08-15-a-relation-is-a-link-with-a-kind.proposed.md)

Three ways to draw the refer picker, built to be argued with rather than
shipped. **Open `index.html`** — the three are tabs.

Everything here is throwaway. The data is invented; the CSS is glance's own,
lifted from `Theme/Default.hs` and `Page/Style.hs` so the rehearsal is judged at
the real metrics and the real hues.

| file | what it draws |
| --- | --- |
| `index.html` | the tabbed shell; each rehearsal runs in its own `<iframe>` |
| `a-modal.html` | a `.pop-band` over the veil, as the sheet's other popups raise |
| `b-inline.html` | an editor's completion under the caret; the pane keeps the keyboard |
| `c-table.html` | the table-view itself, shrunk: four columns, `/` to filter, RET to choose |

Each is a whole page and opens on its own. The shell mounts them as frames, so
no id, stylesheet or listener from one can reach another.

## The verbs, in all three

- **`@`** — link. Fires only at a WORD BOUNDARY (line start or after
  whitespace), so `dmitry@example.com` stays text.
- **`C-u @`** — relation. Asks the KIND first, then the row. Explicit, so it
  fires anywhere, mid-word included.
- **A selected region** — `@` turns it into the link. Its own words become what
  the link reads as, and its text seeds the search. Nothing is written until a
  row is taken, so ESC costs nothing.
- **The top row is always selected**, so RET takes it. In the kind stage the
  typed text is row one when it is not already a kind, which is how "top row
  selected" and "a new kind costs no configuration" hold together.
- **ESC** walks out one step. Giving up on the target gives up the kind with it.

`c-table.html` borrows the table's keys where it IS the table and nowhere else.
The ROW stage: letters are movement, `/` opens the filter, `RET` commits the box
to a chip and hands the keyboard back, a second `RET` on a row chooses, `ESC`
drops what is half-typed, and `RET` on a row is what chooses. The KIND stage is a
plain vocabulary with no schema and no chips, so typing narrows it directly and
`RET` takes it.

**`DEL` walks the whole strip down, right to left**, and is the only key that
does — `previous-row` is movement and nothing else:

| the strip holds | what `DEL` takes |
| --- | --- |
| text in the open filter box | one character, an ordinary edit |
| text typed outside the box | that text, whole |
| filter chips | the last one, whole (`stripLastToken`) |
| the kind badge | the kind — which is going back to choose it again |
| nothing | the `@` itself, and the picker closes |

The query goes **before** the kind deliberately: the kind stage draws no chips,
so taking the kind first would strand the config's default where no key could
reach it.

Over a selection no `@` was ever written, so the last rung just closes and the
region is left exactly as it was.

**Every filter token is `KEY:VALUE`, and free text has its own key.**
`substring:V` is exactly what a bare `V` means, so one matcher serves both and
every chip on the strip reads as a predicate — typing `wrike` and taking the
first offer lands `substring:wrike`, never a bare word. Writing the key out also
buys a value that may spell a separator under quotes without being read as one.

**`c-table.html` speaks SCHEMA's filter grammar, not a substring test.**
`key:value` over the columns, `-` negating the whole token, `|` splitting a
predicate's value into alternatives that OR, quotes making text literal, and the
metas `*empty*` and `*active*`. Priority reads THROUGH its brackets, so
`priority:A` and `priority:[#A]` are one query. A suggestion list sits under the
box in the renderer's own tier order — the value or key the word already SPELLS,
then the literal, then the keys it opens, then values by prefix, then up to five
whole titles — with a row count on each, taken under the chips already
committed. `RET` on a finished token commits it; `RET` on a bare `key:` merely
opens that column's values. Not carried: `planned` (these rows hold no dates)
and `sort:`, which orders nothing in a picker.

**The kind badge is not a filter and does not read as one.** An applied filter is
a round frost chip; the kind states what is being MADE, so it takes the badge
treatment — squared, its own hue over a wash of itself, as a state pill is drawn.

**A badge column's header sits over its badges' FIRST LETTER.** A pill sets its
text in from the track edge by its own padding, so a header aligned to the track
is a header sitting a padding's width left of the words a reader is scanning.
The header takes the same inset. This one landed in the real renderer too —
`.tv-fill th.tv-badge .tv-hd` in `../../../assets/table-view.js`, with
`test/browser/cases.mjs` case 13 and `BREAK=badge-head` behind it.

**The cursor is a ground and nothing else.** A state or priority pill keeps its
own hue and its own wash on the cursor row; the band sits under the cells rather
than repainting them.

`c-table.html` additionally reads `views.default` from the tree's config
(`Config.hs` `defaultFilter`, falling back to `builtinFilter =
"state:*active*"`) and applies it before you type. `marks`, `flags` and
`actions` are table-view's OWN mount options; the picker ships all three off,
and the checkboxes in its rig turn them on so the option's owner is visible.

## Running the cases

Real browsers, no dependencies: node's global `WebSocket` onto CDP for Chromium
and WebDriver BiDi for Firefox, the same shape as `test/browser/drive.mjs`.

```sh
node run-firefox.mjs  a-modal.html a-modal-cases.mjs
node run-chromium.mjs c-table.html c-table-cases.mjs
node shell-check.mjs                 # the tabbed shell mounts all three
```

A case file is written once and runs on either engine — `run-chromium.mjs`
holds the adapter behind the two names (`eval`, `keys`) the cases use.

Last run: **A 21/21, B 25/25, C 41/41, on both engines.**

## Two harness traps, recorded because each looked exactly like a page bug

- **Synthesising `@` with the Shift modifier folded into one event** hides the
  separate `Shift` keydown a keyboard really sends — which is what broke the
  original `C-c @` chord. `a-modal-cases.mjs` asserts the page observes
  `Shift,@`.
- **`rawKeyDown` fires the JS event and performs no edit**, so Backspace deleted
  nothing in Chromium while Firefox was fine. Editing keys need `keyDown` and a
  virtual key code (`run-chromium.mjs`, `AS_CDP`).

A third, in the shell: a URL differing only in its `#hash` is a same-document
navigation, so cases silently share state. The runners add a per-case query.
