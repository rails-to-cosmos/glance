# Bug — every dropdown in the native window is white on white

**Status:** fixed · **Reported:** 2026-08-17 · **Surface:** the native
WebKitGTK window (`make native`) · **Fixed in:** `src-web/Glance/Web/Theme.hs`

## Symptom

In the native app, every `<select>` — the theme picker, the config layer, and
the mint form's namespace and group — draws with a white ground and white text.
The options are unreadable. The browser build looks right.

## Steps to reproduce

```
make native && make run-native      # against the fix's parent, 4a35aaa
```

Open the settings sheet (`,`) and look at the theme dropdown under a dark
theme.

## The cause

**Nothing told the platform the page was dark.** `color-scheme` was declared
nowhere in the served CSS (`src-web/Glance/Web/Theme.hs`, `src-web/Glance/Web/Page/Style.hs`).

A `<select>` is painted by the UA, not by the page. Absent a declared scheme
the UA paints its control ground from its **light** palette — and glance's own
`color` is inherited over it (`.cview` at `Page/Style.hs:266` sets
`color:inherit`, which resolves to `--g-fg`, white under a dark theme). White
text on the UA's white control.

Chromium happens to honour the explicit `background:var(--g-bg)` on
`#themesel,#clayer,#nspace,#ngroup`, which is why the browser build hid it;
WebKitGTK paints the control itself and the page's background never shows.

## The fix

`color-scheme` now rides the palette blocks in `themeCSS`, so every block that
carries a theme's tokens carries its scheme beside them and the two cannot
drift:

```
:root                       { color-scheme:light; … }
@media (prefers-color-scheme:dark) { :root { color-scheme:dark; … } }
:root[data-theme="light"]   { color-scheme:light; … }
:root[data-theme="dark"]    { color-scheme:dark;  … }
```

## What catches it going wrong

- `test/TestServe.hs` — "every palette block declares the scheme the platform
  paints controls in", over the served CSS, all four blocks by exact text.
- `test/browser/cases.mjs` — "every dropdown declares the scheme its platform
  paints it in": the computed `color-scheme` follows `data-theme` both ways, and
  no dropdown resolves one colour for both its ink and its ground. Without the
  fix a dark page computes `"normal"` and the case fails on that word.
- `AGENTS.hs` carries the rule as a `[Test, Browser]` note.

**Verified as far as this machine allows**: the native binary's own served CSS
carries all four blocks. GTK's actual paint was not observed — that needs the
window open on a display, which the fix's author declined to do on the user's
active desktop.
