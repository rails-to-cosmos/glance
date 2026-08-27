# Proposal — every headline carries its creation time

**Status:** proposed · **Date:** 2026-08-27 · **Origin:** user — *"on capture
add a property that reflects created-at as a passive org timestamp, in both
repos; propose a migration so all headlines have it."*

## The property already exists; the coverage does not

`ORG_GLANCE_CREATION_TIME` is org-glance's own creation property, and its
value already IS a passive (inactive) org timestamp —
`[2021-09-13 Mon 23:14]`. glance already stamps it on an INBOX capture
(`captureEdits`, `Query.hs`), already preserves it, and already reads it as a
custom column (`customCell`). So the format, the write path and the read are
in place. What is missing is coverage and consistency:

- Over `~/sync` today: **2825 of 6110 blobs carry it (46%)** — the 2021-era
  ones a prior org-glance stamped; the rest, including freshly captured
  blobs, have none.
- org-glance's current `graph:capture`
  (`src/data/org-glance-graph.el:1780-1792`) ensures `ORG_GLANCE_ID` and
  **nothing else** — it does not stamp a creation time.
- glance's TAGGED capture (the blob mint) does not stamp it either; only the
  inbox jot does.

So a "created" sort or column — the navigation the user wants — is blind on
more than half the store.

## The property, decided

**Reuse `ORG_GLANCE_CREATION_TIME`.** It is already a passive timestamp,
already glance-preserved, already a legal custom column, and 2825 rows carry
it. Introducing org-expiry's `CREATED` would fork a second creation property
and strand those 2825 — rejected. The glance table may LABEL a column
"Created"; the stored key stays org-glance's.

## Part 1 — both repos stamp it on capture

- **org-glance** (`graph:capture`, before `graph:add`): one `org-entry-put`
  of `ORG_GLANCE_CREATION_TIME` to an inactive `(org-time-stamp-inactive)`
  when the headline lacks one, beside the `ORG_GLANCE_ID` it already ensures.
  Idempotent — a headline that already carries one keeps it.
- **glance**: the tagged-capture mint path stamps it the way the inbox path
  already does (`captureProperty`/`captureStamp` are in hand), so whichever
  door mints a blob, the property is on it. The two stamps are the same
  format (`captureStamp`, `TimestampInactive`), so a capture from either side
  reads identically.

## Part 2 — the migration: backfill every headline that lacks it

A one-shot over the store, idempotent and dry-run-first, that sets
`ORG_GLANCE_CREATION_TIME` on every headline missing it — from the best
evidence available, honest about precision:

1. **present** → keep it, touch nothing.
2. **earliest `:LOGBOOK:` stamp** (a state change or clock line) → the
   headline's own recorded history, the truest creation floor.
3. **the blob's file mtime** (for a `data/<shard>/…/data.org`) → the store's
   own timestamp when history is silent.
4. **the migration run's day** → the last resort, and the report NAMES how
   many rows fell to it, so a wall of migration-dated rows is never mistaken
   for real creation times.

Home: a **glance** command — it has the batch write path (a property is a
drawer edit through `replaceSpans`, the same optimistic-lock write every
command uses) and the walk. Shape: a CLI subcommand `glance backfill-created
[DIR] [--dry-run]` (dry-run reports the counts per evidence tier and writes
nothing), or, if the daemon should drive it, a `POST /command
{name:"backfill-created"}` over marked/all rows. It nudges each written path
and appends the usual `EXTERNAL.jsonl` line, so org-glance adopts the writes
on its next refresh — the same contract every glance write uses. Counts ride
the report the way `glance doctor`'s do.

org-glance's side needs no separate migration: glance rewrites the org files,
org-glance re-derives from them. A mirror `org-glance-graph:backfill-created`
is offered for the Emacs-only user who never runs glance — same evidence
tiers, same idempotence.

## Part 3 — the navigation payoff

Once coverage is universal:

- `columns:ORG_GLANCE_CREATION_TIME` already works (custom column); the
  proposal adds a friendly `created` alias and a "Created" header so
  `columns:created` and a labelled column read cleanly.
- Make it **sortable**: `sort:created` orders by the timestamp (the sort
  machinery reads ISO/timestamp keys already for scheduled/deadline; the
  creation stamp joins them). Then "navigate later by when I made it" is one
  token.

## Risk

- **A whole-store write.** Every headline lacking the property is rewritten
  (one property line added inside its drawer; every other byte identical,
  the write law). Dry-run first; idempotent; per-file optimistic lock; the
  `EXTERNAL.jsonl` note so org-glance stays in step. Reversible only by
  removing the property — so the report states exactly what it touched.
- **Evidence honesty**: tiers 3–4 are approximations; the report counts them
  so a reader knows which creation times are real.
- Cross-repo: the capture change is two small edits (one elisp
  `org-entry-put`, one glance mint-path stamp); the migration is glance's,
  with an org-glance mirror for the Emacs-only path.

## Oracle

- glance: TestQuery/TestServe — a tagged capture carries
  `ORG_GLANCE_CREATION_TIME` (inactive timestamp); a capture over a headline
  that already has one keeps it; the backfill sets it from a LOGBOOK stamp,
  from an mtime, and falls to the run day, reporting each tier; a second run
  writes nothing (idempotent); `sort:created`/`columns:created` order and
  show it.
- org-glance: an ERT that `graph:capture` puts the creation time and that a
  re-capture keeps the first.
- interop: a headline captured in Emacs and one in glance both carry the
  property, same format; `make interop` stays green.

## Alternatives

- **`CREATED` (org-expiry's standard)**: cross-tool, but forks a second
  creation property and strands the 2825 existing `ORG_GLANCE_CREATION_TIME`
  — rejected.
- **Derive creation from the org-glance WAL** rather than a property: the WAL
  records revisions, not a creation instant, and 54% of blobs have no early
  WAL record at all — the property on the headline is the durable home.

Inert until reviewed.
