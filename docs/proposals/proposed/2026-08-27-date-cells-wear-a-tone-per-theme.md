# Proposal — date cells wear a tone against today, configurable per theme

**Status:** proposed · **Date:** 2026-08-27 · **Origin:** user — *"deadline
value should be red if passed"*, *"scheduled should be green if today, white
if future, red if past"*, *"both should be configurable per theme."*

## The law in one line

A date cell carries a TONE the server computes against the request's own day
— `overdue` / `today` / `future` — and the tone is a per-theme CSS variable,
defaulting to red / green / the ordinary ink, editable in the settings sheet
beside the state hues.

## The tone, per column

| column | past | today | future |
| --- | --- | --- | --- |
| `scheduled` | `overdue` | `today` | `future` |
| `deadline` | `overdue` | `today` | (none — ordinary ink) |

The user asked deadline red only when passed; `deadline` today takes the
`today` tone too (a due-today row earns a mark), future stays plain. Both
mappings are data (`viewColumns`), so a reviewer can move a tone without
touching the mechanism. An empty date cell wears no tone.

## Why a tone and not a colour

The state and priority columns are `badge` type: each carries a `badges` list
mapping a value to a colour that is a per-theme CSS var
(`overridable "state" KW slot` → `var(--g-state-KW, var(--g-state-a0))`,
`Query.hs`), and `GLANCE_STATE_COLORS: THEME KW=HUE` defines the var per theme
(`Theme.hs`), the settings sheet editing it with a picker. Dates are not
enumerable, so a value→colour list cannot map them. The cell needs a tone the
server picks per row, and the renderer must honour it.

## The one cross-repo change (named, as the org-table and reading-scroller
##  proposals name theirs)

`assets/table-view.js` is vendored. A `text` cell today renders `esc(raw)`
with no per-cell colour. The addition upstream: a cell value may be
`{ text, tone }`, and the renderer wraps a toned cell as
`<span class="tv-tone" style="--tv-tone:var(--g-date-TONE)">…</span>` (the
same shape the badge pill already uses for `--tv-badge`). No enumeration, no
new column type; a `tone`-less cell renders exactly as today. This is the
sibling-repo item this proposal carries.

## The model

- `Query.hs`: `data DateTone = Overdue | DueToday | Upcoming` (or the three
  words); `dateToneOf :: Day -> Text -> Text -> Maybe Text` — the request's
  day, the column key, the ISO cell → the tone word or none, by the table
  above. `rowJSON` emits the date cells as `{text, tone}` when a tone
  applies. The request's day is the one `*today*`/date comparisons already
  use (`Filter.onDay`), so "now" stays the server's local day and a cell's
  tone is stable for the request.
- `viewColumns` gains the per-column tone map beside each date column.

## Per-theme config, mirroring the state hues

- Pragma `#+GLANCE_DATE_COLORS: THEME overdue=HUE today=HUE future=HUE`, one
  line per theme, read like `GLANCE_STATE_COLORS` (every line, last spelling
  wins, shape-validated only). `ConfigParts` gains `cpDateColors`; a
  `ConfigSetting "date-colors"` with `dateColorsEdits`/`dateColorsOf` twins
  of the state pair.
- Theme tokens: `--g-date-overdue`, `--g-date-today`, `--g-date-future` on
  every palette (`Theme/Types.hs` `Palette` gains three fields;
  `Theme.hs` emits them, and `themeOverrides` layers the pragma). Defaults:
  overdue = `pBad` (the one red), today = `pOk` (the landed-write green),
  future = `pFg` (ordinary ink — "white" in the dark theme, black in light).
- Settings sheet: the hues panel (`50-settings.js` `showHues`, `Page.hs`
  `#chues`) gains a `date` group with three pickers per theme, saved through
  the same `configWrites` path the state hues use. The tokens are vars, so a
  theme switch recolours without a refetch — the state-hue property exactly.

## Defaults, stated

Out of the box, no config: overdue red, today green, future the ordinary
ink. A tree that never sets `GLANCE_DATE_COLORS` sees the sensible colours;
a theme author overrides any of the three.

## Oracle

- TestQuery/TestFilter: `dateToneOf` over a fixed day — a scheduled cell
  yesterday is `overdue`, today `today`, tomorrow `future`; a deadline
  tomorrow is toneless; an empty cell toneless. The request-day is the
  fixture's, not the wall clock (pass the day, as the comparison tests do).
- TestServe: `/headlines` over a fixture with a past and a future date shows
  the date cells carrying `tone`; the view JSON round-trips it; a
  `GLANCE_DATE_COLORS` pragma moves the theme token; the settings sheet edits
  a date tone and the write lands on the pragma.
- Browser: a past deadline cell computes to the `--g-date-overdue` colour, a
  future scheduled to `--g-date-future`, today's to `--g-date-today`; a theme
  switch recolours without a refetch; the settings picker changes it live.
  (The toned-cell render rides the vendored change; until it ships, the stub
  asserts the server EMITS the tone.)

## Alternatives

- **Glue post-pass** over the rendered date cells (parse the text, compare,
  stamp a class): reaches into the renderer's DOM, re-runs every re-render,
  and duplicates the server's date parse — rejected, it fights the vendored
  boundary rather than extending it.
- **A CSS-only rule** on a column class: the tone is per-row, not per-column,
  so CSS alone cannot see "past" — rejected.

## LOC, roughly

+40 Query (tone + column map + config twin), +20 Theme (three tokens ×
themes + pragma), +30 settings glue + Page markup, +3 docs; the renderer's
per-cell tone upstream (~15 lines there). Config and theme ride the exact
rails `GLANCE_STATE_COLORS` laid.

Inert until reviewed.
