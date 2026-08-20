# Bug — the priority badge wears an ellipsis it cannot need

**Status:** fixed · **Reported:** 2026-08-15 · **Browser:** Chromium
· **Fixed in:** `../table-view/web/table-view.js`, synced to `assets/table-view.js`

## Symptom

In the table's priority column, a badge draws as `[#B]…` — the whole pill, and
an ellipsis behind it. The pill is not cut short; the ellipsis is simply added
after a badge that fits.

Only the priority column shows it. `state` is a badge column too and draws
`TODO`, `NEXT`, `STARTED` clean.

## Steps to reproduce

1. `git checkout eaf1f27 -- assets/table-view.js` (any tree before the fix).
2. `cabal build exe:glance`
3. `$(cabal list-bin exe:glance) serve --dir test/browser/tree --port 7777`
4. Open `http://127.0.0.1:7777/` in Chromium.
5. Read the `#` column on the row `A priority the badge column paints`
   (`test/browser/tree/plans.org:16`).

The reading, taken in the page:

```js
[...document.querySelectorAll("#app td .tv-pill")].map((n) => {
  const td = n.closest("td"), cs = getComputedStyle(td);
  return { text: n.textContent,
           pill: n.getBoundingClientRect().width,
           // FRACTIONAL — clientWidth is a rounded integer and hides the verdict.
           inner: td.getBoundingClientRect().width
                - parseFloat(cs.paddingLeft) - parseFloat(cs.paddingRight) };
});
```

| badge | pill | cell's content box | verdict |
| --- | --- | --- | --- |
| `[#A]` | 47.203px | 47.188px | **0.015px short** |
| `TODO` | 47.203px | 70.594px | fits |
| `NEXT` | 47.203px | 70.594px | fits |

Same reading against the real corpus (`~/sync/views`, 6109 files) at 1400×900,
1920×1080 and 800×900: `[#A]` short at every width, `TODO`, `STARTED` and
`PENDING` clear by 24px.

## Evidence

- `assets/table-view.js:1798-1800` — `.tv-pill` spends **16px** of ground
  (`padding:0 8px`).
- `assets/table-view.js:1057` — the allowance was `PILL_CH = 2`, in
  **characters**. At the renderer's 13px monospace face 1ch is 7.80075px, so two
  are **15.6015px** against the 16px actually spent: **0.3985px short by
  construction**, at that face, before any rounding.
- `assets/table-view.js:1576-1578` — `.tv-fill td` carries
  `overflow:hidden;text-overflow:ellipsis`. The pill is an `inline-block`, an
  atomic inline `text-overflow` cannot cut, so what Chromium draws is the whole
  badge **and** the ellipsis. That is the `[#B]…` on screen.
- `src-query/Glance/Query.hs:1801` — `("priority", "#", "badge", hrPriority)`.
  The header is one character, so under the fill policy the **cell** sets the
  width and the column is sized at exactly the pill's own measure. Every other
  badge column is sized by a word longer than its pill and hides the deficit —
  which is why the bug reads as the priority column's alone.

The remaining 0.015px, past the 0.3985px above, is the engine's own: a column
width lands on Chromium's 1/64px `LayoutUnit` grid, and it lands **down**.

## Fix

`PILL_CH` (characters) → `PILL_PAD = 17` px, and the width cache split into the
two units it was always spending: `{ch, ground}`, text in `ch` and grounds in
`px`.

- `assets/table-view.js:1070` — the constant, 16 for the padding and one for the
  1/64 grid.
- `assets/table-view.js:2033` — `widths` is now `{ch, ground}[]`.
- `assets/table-view.js:2787` — the ground is bought only where a pill draws, so
  a badge column holding no cell keeps the bare one.
- `assets/table-view.js:2804` — `growWidths` buys it when the first cell arrives.
- `assets/table-view.js:2827` — `applyWidths` writes `calc(Nch + ground px)` and
  sums the same grounds into the table's `min-width`.

After: `[#A]` measures 47.203px in a cell holding 48.188px, at every width.

## What catches it going wrong

- `test/browser/cases.mjs:469` — case 12, every pill on the page against its
  cell's **fractional** content box.
- `test/browser/drive.mjs:96` — `BREAK=pill-ground` widens `.tv-pill` past the
  ground and turns case 12 red:
  `the "[#A]" pill measures 59.2px in a cell holding 48.2px`.
- `AGENTS.hs:2999` — the rule, `[Browser]`.
