# Proposal — `/` filters, `.` composes the whole expression

**Status:** done — DELIVERED 2026-08-21 · **Date:** 2026-08-20 · **Origin:**
user — *"maybe make kbd `.` a general dsl expression (what is currently on
`/`), and restrict `/` to filters only."*

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

## As delivered

The split landed as proposed: `/` is `filter-rows` over the filter half, `.` is
`compose-query` over the whole expression — one new `Keymap.hs` row, `.` having
been unbound — and both doors read and write the one `?q=` string.

- **The narrow door is a mount call, never a second box.** `openFilter()` is
  unchanged, the whole grammar, so every existing caller means what it meant;
  `openFilter({narrow: true})` opens the same box on the filter half. The key
  stage offers the narrowing keys alone — the six column keys, `planned`,
  `ref`, `substring` — and the shaping keys `sort:`/`columns:`/`view:` never
  appear in it, while the `+` sign, the metas and every value stage work as
  they do everywhere. The session's input placeholder names the filter half;
  the whole door's placeholder is untouched.
- **A refused token stays where it was typed.** Committing a narrow session,
  a token whose key is `sort`, `columns` or `view` — either sign — is never
  chipped and never reaches the delivered query, and it is left standing in
  the box, so the reader sees the words that were refused. The mount gains one
  optional option, `onRefused(spelling)`, called once per refused token with
  the token's source text; the shell speaks it, and the echo names the other
  door.
- **Standing shape rides along.** The chips are the strip, not the box: a
  narrow commit leaves an applied `sort:`/`columns:` untouched, so narrowing
  never loses the order.
- **The flag is the SESSION's.** It clears when the box closes, so an
  `openFilter()` after an `openFilter({narrow: true})` is the whole door
  again — the restriction is a view over one query, never a mode the box
  keeps.
- **What stayed.** The picker mount's own `/` still summons the whole door
  (`openFilter()`, no options): a picker narrows over its own vocabulary and
  has no shaping half to lose. The links/tags popups' `/` is the different
  surface the frictions above named, unmoved. `.` binds in the `table` scope
  alone, so the document pane keeps its keys.
