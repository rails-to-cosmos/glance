# Bug — the renderer reads the added sign as text

**Status:** fixed · **Reported:** 2026-08-20 (review of the additive-filters
delivery) · **Surface:** the filter box and the chip strip, vendored
`assets/table-view.js` · **Fixed in:** `../table-view/web/table-view.js`
(synced to `assets/table-view.js`), `frontend/glue/00-core.js`,
`frontend/glue/30-capture.js`, `frontend/glue/60-refer.js`

## Symptom

The renderer has no `+` in its grammar, so the sign is body text everywhere
the page reads a query, and the divergence goes past matching:

- taking the completion over `+web` writes `substring:+web` — a token
  narrowing to rows that literally carry the characters `+web`, a dead
  literal;
- the chip strip draws `milk +bread` as `substring:milk` and
  `substring:+bread` — the widening spelled as a narrowing;
- a `+` token is offered no key and no value, where the same token under `-`
  completes and gets its sign back.

## Steps to reproduce

Serve any tree and open the page.

1. `/`, then type `+web` and press RET on the first row. The box reads
   `substring:+web` and the table serves nothing; `-web` accepted the same way
   comes back as `-substring:web`.
2. Type `milk +bread` and commit. The strip draws two chips, `substring:milk`
   and `substring:+bread`, so a reader is told the second word narrows.
3. Type `+sta`. The list holds the text-search literal alone, where `sta`
   offers `state:` and the keywords behind it.

## Evidence

- `assets/table-view.js:325` — the scanner's sign branch is `-` alone
  (`} else if (!seen && c === "-")`), so a leading `+` lands in the token's
  body and `t.negated` is the only sign a token carries.
- `assets/table-view.js:3492` — `stageAt` reads a keyless token as the key
  stage off `t.value`, sign included, so the prefix every suggestion opens on
  is `+web`: no key and no column domain can match it.
- `assets/table-view.js:3646` — `literalOffer` spells that prefix
  `substring:+web`, and `acceptAc` (`:3743`) rebuilds the head as
  `t.negated ? "-" : ""`, restoring the `-` and never the `+`, so accepting
  the offer commits the dead literal.
- `assets/table-view.js:3193` — `spelled` labels a keyless token
  `${t.negated ? "-" : ""}substring:${value}`, which is how `+bread` reaches
  the strip as `substring:+bread`.
- The matching half of the divergence is declared rather than filed:
  `AGENTS.hs:2597`, `(AddKey, Renderer)` — "the renderer reads `+state:B` as
  free text, so it answers with fewer rows". The proposal's implementation
  sketch (`docs/proposals/done/2026-08-20-additive-filters.md`) called that gap
  row the whole renderer story; completion and the strip are the part it
  missed, where the sign is rewritten into text the reader never typed.

## Fix

The renderer carries the sign, ported from the law in `docs/query.md`,
"Adding: `+` widens its own axis", and from the server's own
`compile`/`axisTest` (`src-web/Glance/Web/Filter.hs`):

- the scanner's sign branch reads both signs, so a leading `+` is the token's
  sign instead of the first character of its body, and a token states which
  of the two it wears;
- `spelled` writes the sign ahead of the key it prints, so `+bread` reaches
  the strip as `+substring:bread` and a keyed `+priority:[#B]` stands as
  written — every chip the token the reader typed;
- `stageAt` opens the key stage on the value behind the sign, so `+sta`
  offers `state:` and the keywords under it, and `acceptAc` rebuilds the head
  with whichever sign the token wore — the `+` restored where only the `-`
  used to come back;
- the shaping keys drop with a sign on them: `+sort:`, `+columns:` and
  `+view:` are no local predicate and name no saved view, which is the 400
  the server answers;
- the local matcher answers the axis law — per key the plain and negated
  tokens AND with the negation inverting inside that conjunction, the added
  tokens OR against it, the axes AND, and a non-negated token naming no atom
  establishes no axis — so the page and the daemon serve one query.

The three glue guards gained the added twin of their negation test. The
parity check's loose-token scan (`frontend/glue/00-core.js`) skips an added
token, which is no evidence of a dropped key; its `+`-prefix byte guard
stands beside the new test, still covering a stale asset that reads the sign
as text. `splitKind` (`frontend/glue/60-refer.js`) leaves an added `kind:`
token in the query — it is no picker state, and it rides to the server as the
widening it spells. `filteredTags` (`frontend/glue/30-capture.js`) passes an
added `tag:` over: a widened tag is an alternative rather than a facet every
shown row carries, so it seeds no capture tag.

Upstream-first, as AGENTS.hs's sync-renderer note requires — "Editing the
vendored copy by hand is a fork". `../table-view` had no `web/` tree at all,
so the source was restored there from the sole surviving copy, the vendored
`assets/table-view.js`; the sign landed in the restored source and
`make sync-renderer` carried it back into `assets/`. Sibling and vendored
copy are one file again, so the sync is live.
