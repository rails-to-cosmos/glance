# Proposal — `/` filters, `.` composes the whole expression

**Status:** draft · **Date:** 2026-08-20 · **Origin:** user — *"maybe make
kbd `.` a general dsl expression (what is currently on `/`), and restrict
`/` to filters only."*

## The split

- **`/` — `filter-rows`, filters only.** The box edits the FILTER half of
  the current query; the standing `sort:`/`columns:` ride along untouched,
  so narrowing never loses the shape. Placeholder and completion offer the
  narrowing keys alone (the six columns, `planned`, `ref`, `substring`,
  metas — and `+` when additive filters land). A shaping token typed here
  is a spoken refusal naming the other door: `sort: belongs to .` — org
  agenda's own `/`, vim's own `/`: search narrows, never reorders.
- **`.` — the whole expression.** The full pipeline, raw:
  `filter(...) sort: columns:` with schema-aware completion in three
  sections. What `/` is today, under a new key; `.` is unbound everywhere
  (no Keymap row, no glue handler).

One state underneath: both doors read and write the one `?q=` string; `/`
is a restricted VIEW over it, never a second query.

## Why

- Progressive disclosure: the common gesture (type words, narrow) gets an
  honest search box with a short completion list; the power surface gets
  the whole grammar without crowding the common path.
- The relational reading (additive-filters proposal): `/` edits the
  `filter(...)` stage, `.` edits `filter().orderBy().select()`.
- Commands stay honestly named: `/` keeps `filter-rows`; `.` becomes
  `compose-query`.

## Sketch

- `Keymap.hs`: one new row — `bind ["."] "compose-query" (Just
  "focusQuery") "table"` — plus help strings; `keyHints` gains it.
- Glue: `focusFilter` keeps the box but swaps placeholder/completion to
  the narrowing subset and refuses shaping tokens on commit with the echo;
  `focusQuery` is today's `focusFilter` whole. The splitter reuses the
  server's own token scan (`scanQuery`'s law) — a shaping token is one
  whose key is `sort`/`columns`/`view`.
- The `/`-commit recomposes: typed filters + preserved shaping tokens, in
  the query's existing order.
- Tests: TestServe — `/` preserves an applied `sort:` across a filter
  edit; `sort:x` typed into `/` refuses with the echo and writes nothing;
  `.` round-trips the whole string; completion lists differ between the
  two doors. The keymap table gains the row (TestServe `expectedRows`).

## Frictions accepted

- Muscle memory: `sort:` typed into `/` worked yesterday; the refusal
  echo teaches the new door in one keystroke.
- The links/tags popups' own `/` (narrow-within-popup) is a different
  surface and does not move.

Inert until reviewed.
