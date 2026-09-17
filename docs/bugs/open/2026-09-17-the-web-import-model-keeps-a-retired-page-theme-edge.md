# Bug — the web import model keeps a retired page theme edge

**Status:** open · **Reported:** 2026-09-17 · **Surface:** the executable
architecture model in `AGENTS.hs`

## Symptom

The full Haskell suite fails in `Build and discipline / the door is read by the
desktop pair alone`. The model says `Glance.Web.Page` imports
`Glance.Web.Theme`; the module no longer has that import.

## Steps to reproduce

1. Run `make test`.
2. Read the failure headed `the web layer's import graph`.
3. Compare the expected `Glance.Web.Page` edges with its imports.

## Evidence

- `AGENTS.hs:6140` includes `WTheme` in `wimports WPage`.
- `src-web/Glance/Web/Page.hs:4-13` imports `Base`, `Keymap`, `Page.Glue`,
  `Page.Popups`, and `Page.Style`; it does not import `Glance.Web.Theme`.
- `test/TestSpec.hs:1737-1739` compares every modeled web edge with the real
  module imports and reports the mismatch.

