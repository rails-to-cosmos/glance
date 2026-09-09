# Bug — the drawer fold sign is a fixed ink, not its spine's

**Status:** fixed · **Filed:** 2026-09-09 · **Fixed:** 2026-09-10 · **Surface:** the material doc's
drawer fold sign (`.fold`) against the block spine it sits on.

## Symptom

The clickable `+`/`−` fold sign on a drawer's spine is drawn in a fixed ink
(`--g-mark`), so it matches the spine only when the spine happens to be that
ink. Where the spine is dim (`--g-point-off`, a resting block) or an accent (the
ramp a nested block wears), the sign stands a different colour from the spine it
is attached to.

## Root cause

- The spine's colour is `var(--spine)`, contextual: `--g-point-off` on a resting
  block (`assets/page.css:438`), `--g-mark` at `sp-0` (`:462`), the accent ramp
  deeper.
- `.fold` hard-codes `border-color: var(--g-mark)` and `color: var(--g-mark)`
  (`assets/page.css:632`,`:635`), and lifts to `--g-fg` at point
  (`:647`) — none of it read off `--spine`.

## Fixed (2026-09-10)


`.fold` reads `var(--spine)` for its border and ink, so it tracks the spine in
every state, and the point-lift override is dropped (the spine lifts itself, and
`.fold` follows). The hover accent stays — it is an interaction affordance, not
the resting colour.
