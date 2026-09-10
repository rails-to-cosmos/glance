# Bug — the git control vanishes when the view is re-applied

**Status:** fixed — redesigned; the control moved to a shell-owned row · **Reported:**
2026-09-10 (user: *"g on main page moves pin to the right and git widget hides"*) ·
**Surface:** `frontend/glue/80-git.js` (the injection), table-view's `renderChips`
and re-mount

## Symptom

Pressing `g` (apply the view) makes the git control disappear and the pin spring
back to the right; the control returns only after the next 15 s poll — "very
unstable."

## Root cause

The control was injected into `.tv-chips`, a subtree **table-view owns and
reclaims** two ways: `renderChips()` sets `chipsEl.innerHTML` on every chip
render (wiping the injected node), and `g` re-mounts the table with a *fresh*
`.tv-chips` element (orphaning the observer on the old one). Every fix — a
strip observer, then an `#app` observer, then inline-style re-apply for the pin,
then the poll — was a reaction with a gap; the poll's 15 s gap is the visible
flicker. A node in a component's owned, rewritten subtree is transient by that
component's contract.

## Reproduce

`spikes/2026-09-10-git-control-placement/index.html` — select **Old (injected)**,
press **re-mount table (g)**: the verdict reads `control present: no`. Under the
shell-owned placements (A/B/C) both re-render and re-mount leave it `yes`.

The browser suite serves a non-git fixture tree, so `/git` answers `repo:false`
and the control never mounts there — it is not yet exercised by `test/browser/`.
See [[../../tasks.org]] for the harness git-fixture follow-up.

## Fix

The control now lives in `#ghead`, a shell-owned row above the table
(`Page.hs`), OUTSIDE `#app`. table-view never touches it, so a re-render or
re-mount leaves it in place — consistent by construction, no observers, no pin
CSS fight. `80-git.js` drops the inject/observe/place/detach machinery; it fills
`#ghead` on a `/git` poll and clears it (CSS `:empty` collapses the row) off a
non-repo dir.

- `src-web/Glance/Web/Page.hs` — `<div id="ghead"></div>` in the skeleton
- `assets/page.css` — `#ghead` right-aligned, `:empty` collapses
- `frontend/glue/80-git.js` — the control renders into `#ghead`
