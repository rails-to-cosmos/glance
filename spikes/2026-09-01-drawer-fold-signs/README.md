# Spike — the boxed +/− fold sign on a drawer's spine

**Date:** 2026-09-01 · **After:** the mouse work — a click selects an element, a
double click edits it — where a drawer got no visible, mouse-reachable fold.

The **boxed +/−** was chosen over the chevron, the spine dot, the lead chevron
and the +N pill. This spike shows it in the REAL material doc: a headline, its
planning line (with an unset DEADLINE slot drawn as a muted dash), a foldable
properties drawer, a paragraph, and a list with a checkbox — the palette and the
`.d-*` geometry lifted from `Theme/Default.hs` and `Page/Style.hs`, so the sign
is judged at the real hues, the real spine column (`--rail`), and the real
metrics. **Open `index.html`** — click the box on the drawer's spine to
fold/expand; the **theme** button judges both palettes.

The look: a 12px box at the drawer's `--rail`, its border in the row's `--ink`
(the same bar the drawer's spine wears) and its ground the page's, so the spine
reads as passing behind it. Shut it reads `+` and the drawer is its
`:PROPERTIES: …` summary; open it reads `−` and the pairs stand.

Throwaway. The real one becomes a clickable hit-target on the drawer's spine in
the material doc — a fold toggle through `TAB`'s own door (`kind: "tab"` at the
drawer's row) — built failing-test-first.
