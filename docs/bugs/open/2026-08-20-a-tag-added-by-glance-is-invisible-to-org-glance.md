# Bug — a tag added by glance is invisible to org-glance

**Status:** open · **Reported:** 2026-08-20 (live use: `:youtube:` added, org
file updated, org-glance never sees it) · **Surface:** `add-tag` → org-glance
`graph:tags`

## The mechanism

`add-tag` splices the tag and, on success, appends one EXTERNAL.jsonl line —
`{"id":…,"at":…}` (`Commands.hs:115`, `Query.hs:1105`, `External.hs:70-76`).
org-glance discovers tags by folding live headline records, so a NEW tag rides
in on its headline; what a tag needs is the headline's IDENTITY:
`refresh-external` re-derives metadata only for ids its store already knows
(`org-glance-graph.el:1489-1498`).

## Two failure modes, one diagnostic

- **A — the headline lives in a plain org file.** `externalPathOf` is Nothing
  outside `.org-glance/data/**/data.org` (`External.hs:40-43`), so NO ledger
  line is written, and org-glance has no record and no re-scan path: the tag
  is invisible permanently. Matches the report most closely.
- **B — the headline is a blob glance captured.** The line lands but the id
  is skipped as "unknown or deleted" and the cursor burns it — the KNOWN
  adopt-hole, already specified: `AGENTS.hs` ("The hole is a TAGGED CAPTURE"),
  pinned red-if-fixed at `test/interop/drive.mjs` (~450), prose in
  `docs/plan-org-console-web.md`.
- If the blob predates glance, the tag arrives on the next fold — a long-lived
  Emacs may just be inside `org-glance-graph-external-poll-seconds`; `M-x
  org-glance-graph:refresh-external` settles it.

Diagnostic: is the row's file under `.org-glance/data/`? Did
`meta/EXTERNAL.jsonl` gain a line at tag time? No line → A. Line + a
"skips <id> (unknown or deleted)" message on refresh → B.

## Fixes, smallest first

1. Document the refresh step (covers the throttle case). Zero code.
2. Close the adopt hole in org-glance (`graph.el:1489`): a ledger id whose
   blob exists is adopted rather than skipped — the interop BREAK exists to go
   red when this lands.
3. Mode A has no glance-side fix: a plain-file headline has no record in
   org-glance; the honest paths are "out of org-glance's world" or capture
   into the store first, which lands on fix 2.
4. Separately worth doing, for a different defect: `add-tag` on a tag with no
   config layer leaves it without a `#+TODO:` cycle, so a custom state folds
   into the title on reparse — minting the layer belongs to the settings door
   (`invariants.md`, "one minter"), never to `add-tag`.

## The suite gap

Neither suite issues `add-tag` at all, and the one interop tag is pre-seeded
before capture (`og.el:135-148`). The catching case: after `tag-cycle-
survives`, `add-tag` a never-seen name onto ALPHA, refresh, assert it in
`field.tags` (~10 lines in `test/interop/drive.mjs`).
