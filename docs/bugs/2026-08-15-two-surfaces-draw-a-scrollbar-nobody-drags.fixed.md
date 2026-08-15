# Bug — two surfaces draw a scrollbar nobody drags

**Status:** fixed · **Reported:** 2026-08-15 · **Browser:** Chromium
· **Fixed in:** `src-web/Glance/Web/Page/Style.hs`,
`../table-view/web/table-view.js` (synced to `assets/table-view.js`)

## Symptom

The main page carries a vertical and a horizontal scrollbar. They read as the
browser's own — full-fat OS bars, taking layout space — on a page whose whole
point is that it does not scroll.

## Steps to reproduce

1. `git checkout eaf1f27 -- assets/table-view.js src-web/Glance/Web/Page/Style.hs`
2. `cabal build exe:glance`
3. `$(cabal list-bin exe:glance) serve --dir ~/sync/views --port 7777`
4. Open `http://127.0.0.1:7777/` in Chromium at any window size.
5. A horizontal bar sits under the keybinding strip at the foot, at every size.
   Narrow the window under the table's `min-width` and a second one appears
   under the rows.

The reading, taken in the page — every box whose scrollbar takes layout space,
border discounted:

```js
[...document.querySelectorAll("body *")].flatMap((n) => {
  const cs = getComputedStyle(n);
  if (cs.display === "inline") return [];                       // clientWidth is 0, and meaningless
  if (cs.overflowX === "visible" && cs.overflowY === "visible") return [];
  const gy = n.offsetWidth  - n.clientWidth
           - parseFloat(cs.borderLeftWidth) - parseFloat(cs.borderRightWidth);
  const gx = n.offsetHeight - n.clientHeight
           - parseFloat(cs.borderTopWidth)  - parseFloat(cs.borderBottomWidth);
  return gx > 1 || gy > 1 ? [{ el: n.id || n.className, gx, gy }] : [];
});
```

Against `~/sync/views` (6109 files), before the fix:

| width × height | box | bar |
| --- | --- | --- |
| 1400×900 | `#kbd` | 15px tall |
| 1920×1080 | `#kbd` | 15px tall |
| 1280×720 | `#kbd` | 15px tall |
| 1024×768 | `#kbd`, `div.tv-scroll` | 15px tall each |

**Both measured bars are horizontal.** The vertical one in the report was not
reproduced headless at any size above — `.tv-scroll` measured `gutterY=0`
throughout, this corpus's default view fitting the window. `.tv-scroll` is where
one would come from — it is the rows' own scroller — and the fix below silences
it on both axes, so the report is answered whether or not the exact state that
showed it is reached again. Anyone who does reach it: say what the row count and
the window height were. (`#log` also carries `overflow-y:auto` under a fixed
height, `Style.hs:65-66`, so a long enough log draws one there too; it was clear
at every size measured, and is not covered by the fix.)

## The document was never the one scrolling

`document.scrollingElement.scrollWidth - clientWidth` and its `scrollHeight`
twin are **0 at every size tested**, and `innerWidth -
documentElement.clientWidth` is 0 with it. `body{…overflow:hidden}`
(`src-web/Glance/Web/Page/Style.hs:53`) already propagates to the viewport,
`html` being `visible`, and `test/browser/cases.mjs:153` has asserted it since
the browser driver landed. Both bars belonged to boxes **inside** the page, and
each looks exactly like the browser's because Chromium draws it with the same
widget.

## Evidence

- `src-web/Glance/Web/Page/Style.hs:80` — `#kbd` is `white-space:nowrap` with
  `overflow-x:auto`. The strip is one line of every binding, so it is **wider
  than the window at every size** and the bar was permanent: 15px of chrome
  under a hint nobody drags, at 1920×1080 as much as at 1024×768.
- `assets/table-view.js:1546` — `.tv-scroll` is `overflow:auto` and the sheet
  styles no scrollbar, so the rows' own scroller drew the default pair. The rows
  are driven by key and by wheel; nothing on the page asks the reader to drag.
- A classic bar takes **layout width**, so this also cost geometry: the width the
  vertical bar took came out of the fill (`title`) column, and the sideways
  scroll of a narrow window began a bar's width early.

`#dpara textarea` had already been given exactly this treatment for exactly this
reason (`src-web/Glance/Web/Page/Style.hs:182-189`, "a scrollbar taking layout
width wraps the field narrower than the row under it"). The rule had simply
never been carried to the two surfaces above.

## Fix

Both spellings on both surfaces — Firefox reads the property, Chromium the
pseudo. Each still scrolls; each draws nothing.

- `src-web/Glance/Web/Page/Style.hs:81-82` — `scrollbar-width:none` on `#kbd`,
  and `#kbd::-webkit-scrollbar{width:0;height:0}`.
- `assets/table-view.js:1549` and `:1551` — the same pair on `.tv-scroll`.

After: no box on the page takes scrollbar space at 1400×900, 1920×1080,
1280×720, 1024×768 or 800×900, against the fixture tree and the real corpus.

## What catches it going wrong

- `test/browser/cases.mjs:501` — case 13, every non-inline clipping box on the
  page measured for a bar that takes layout space. Distinct from
  `cases.mjs:153`, which asks whether the **document** scrolls; this asks what
  chrome the surfaces draw, which is a bar's width of layout either way.
- `test/browser/drive.mjs:99` — `BREAK=bar-space` hands both surfaces a 15px bar
  back and turns case 13 red:
  `at 1400x900 a scrollbar takes layout space on #kbd (0px wide, 15px tall)`.
- `AGENTS.hs:4175` — the rule, `[Browser]`.
