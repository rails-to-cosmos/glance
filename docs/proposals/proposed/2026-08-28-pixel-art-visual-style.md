# Pixel-art visual style (Pebble Time 2 direction)

**Status:** proposed · **Date:** 2026-08-28 · experiment
Spike: [`spikes/2026-08-28-pixel-style/index.html`](../../../spikes/2026-08-28-pixel-style/index.html)

## Want

Make glance able to wear a **pixel-art skin** — the chunky, flat, limited-palette
look of Pebble Time 2 game screens. Explore the directions and their cost, not
commit to one yet.

## The seam glance already has

A pixel skin is cheap because glance's styling already routes through the places
you'd hook:

- **Palette** — every colour is a CSS var; `Glance.Web.Theme` supplies
  `themeCSS` + `themeOverrides colours`, and `themeIds` names each theme. A pixel
  look is *one more theme entry* with a reduced palette. No new plumbing.
- **Renderer** — `assets/table-view.js` is vendored and byte-locked, but glance
  styles it from the outside via `#app .tv-*` override selectors in
  `Page/Style.hs`. A pixel theme reaches the list without editing the renderer.
- **Font** — the one exception: the mono stack is set *globally* in
  `Page/Style.hs` (`--glance-mono`, `body{font:…}`), not per theme. A pixel font
  needs a small hook so a theme can swap `--glance-mono`.

So: **pixel = a theme + a font hook + a chrome mode.** Everything else reuses
what's there.

## Directions (independent levers — mix and match)

1. **Bitmap / pixel font** — biggest lever, cheapest. Swap `--glance-mono` for a
   pixel face (Silkscreen, Press Start 2P, Pixelify Sans, VT323). Crisp at
   8-multiple sizes; monospace layout math (`ch` units) still holds.
   *Cost:* pixel faces are wide and blocky — readability drops at glance's text
   density. Silkscreen/VT323 stay legible; Press Start 2P is a poster font only.

2. **Reduced flat palette** — a new theme entry with ~6–16 fixed colours. Drop
   the `color-mix` ramps and soft shadows for flat fills.
   *Cost:* the material-doc's fine typography (sub-pixel spine ramps, gold-on-gold
   selection) is *built on* `color-mix`; pixel mode simplifies that layer to flat
   bars rather than extending it.

3. **Hard pixel chrome** — `border-radius:0`, 3px solid borders, an *offset* hard
   shadow (`4px 4px 0`, no blur), notched corners via `clip-path`, or a 9-slice
   `border-image` sprite for true game-dialog frames.
   *Cost:* `border-image` needs a tiny sprite asset; `clip-path` is free but
   coarser. Gated behind a body class so only the pixel theme pays.

4. **Dither / scanline layer** — decorative overlay (tiled 2×2 pattern or
   `repeating-linear-gradient`, optional scanlines). Pure toggle, no layout cost.

5. **Pixel sprites** — checkboxes (`.dbx`), state keywords, tree connectors as
   tiny sprites. Most art work; leave for last.

## What fights it

The material-doc pane's whole point is *fine* typographic signalling (dim/lit
ramps, spine hues, gold cursor). Pixel art wants the opposite — flat, hard,
few colours. Pixel mode should **collapse** that layer to flat bars, not try to
keep both. Treat it as a mode switch, not an overlay.

## Recommendation

Ship as an **opt-in theme `pixel`** = font hook (lever 1) + flat palette
(lever 2) + a `pixel` body-class enabling hard chrome (lever 3). Levers 4–5 are
later polish. This reuses the theme selector and the `#app .tv-*` overrides
entirely; the only new code is the font hook and the chrome-mode CSS behind a
class. Lowest risk, no renderer touch, reversible (it's just another theme).

## Open questions (for you, after the spike)

- Which font — **Silkscreen is proportional**, so it breaks glance's `ch`-column
  math (log columns, tree rails, `#dkey`). Use it for chrome/labels only, or pick
  a monospace pixel face for the columnar list: VT323 (terminal, legible) or
  Handjet (dot-matrix, variable). A two-font split — Silkscreen headings + mono
  body — is the clean path.
- Palette — winners so far: glance + pebble. Other flat-friendly candidates:
  gruvbox (warm, Emacs-native, legible), amber CRT (monochrome phosphor, pairs
  with VT323), c64 (blue-on-blue, playful extreme). Dark or light ground?
- Font wiring — accept a per-theme `--glance-mono` override in `Page/Style.hs`
  (small change to a global today).
- Chrome — `clip-path` notches (free) vs `border-image` sprite (truer, one asset)?

## Try it

Open the spike in a browser and toggle **font · palette · chrome · dither** to
find the combination that reads right. It mocks a glance list + a popup card so
you see the look on real-ish content, standalone, with zero risk to the app.
