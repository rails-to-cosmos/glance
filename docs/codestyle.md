# Code style

**Status:** doctrine · **Date:** 2026-08-28

Formatting conventions the sources hold to. Values, not bytes: the tests read
CSS whitespace-blind, so these rules serve the *reader*, not the machine.

## CSS (`assets/page.css`)

The one CSS source. It is served after the head strips every blank line and
every line containing `/*`, then replaces the `{{…}}` tokens — so the file is
free to be as readable as it likes; the wire is unaffected.

- **One declaration per line**, indented **4 spaces**.
- **A space after the declaration colon**: `--g-doc-pad: 6px;`, `margin: 0;`.
- **The selector line ends with exactly one space then `{`** (`:root {`,
  `body {`, `#mdoc.on .de.dat {`); the closing **`}` sits on its own line at
  column 0**.
- **One blank line between top-level blocks.**
- **A space after every comma** in a value list; spaces around `calc()`
  operators (CSS requires the latter).
- **Comments: one `/* … */` per physical line**, indented to its block. Never a
  `/* … */` spanning multiple lines — the serve-time strip drops lines
  *containing* `/*`, so a wrapped comment would leak its tail into the page.
- **Law comments** (the ALL-CAPS-lead invariants — `A WHOLE NUMBER OF PIXELS PER
  LINE`, `THE TWO GOLDS`, …) are kept **verbatim**; reflow the layout around
  them, never their words.
- **`{{…}}` tokens** (`{{THEME}}`, `{{LOGN}}`, the `Popups` selectors) stay
  exactly as written and in place — a generic CSS formatter chokes on them, so
  reformat by hand or over token-free regions only.

Canonical shape:

```css
/* Colours live in 'Glance.Web.Theme'; this block is GEOMETRY. */

:root {
    --glance-mono: "JetBrains Mono", "Fira Code", "SF Mono", Menlo, Consolas, monospace;
    --g-doc-pad: 6px;
    --g-doc-padx: 10px;
    /* A WHOLE NUMBER OF PIXELS PER LINE: a 1px hairline and a hinted glyph land */
    /* on one device row only when the row itself starts on the grid. */
    --g-doc-fs: 13px;
    --g-doc-lh: 21px;
}

body {
    margin: 0;
    font: 14px/1.5 var(--glance-mono);
    background: var(--g-bg);
    color: var(--g-fg);
}
```

## Elm (`frontend/elm/`)

Elm conventions live beside the sources, in
[`frontend/elm/CODESTYLE.md`](../frontend/elm/CODESTYLE.md).
