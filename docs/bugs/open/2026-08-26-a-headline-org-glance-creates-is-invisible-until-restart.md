# Bug — a headline org-glance creates is invisible until the daemon restarts

**Status:** open · **Reported:** 2026-08-26 (live use: a headline captured in
Emacs, `~/sync/views/.org-glance/data/91/5358af-…/data.org`, absent from a
daemon up since the day before) · **Surface:** the file watch; every row
org-glance mints into a fresh shard

## The symptom

Emacs captures a headline. org-glance mints its id, `mkdir`s the shard
`data/91/` (a NEW directory — 14:11), then the blob directory (17:03) and
`data.org` (17:57), and appends three revisions to its WAL. The running
daemon answers `404` for the id and serves no row; `X-Glance-Total` never
moves. A fresh `glance scan` of the same tree parses the blob and reports it
in agreement with the WAL — a restart shows the row.

## The mechanism

One `watchTree` on the root (`src-web/Glance/Web/Watch.hs:46`), and fsnotify
arms a newly created directory without traversing into it — measured in
`CHANGELOG.md` (0.5-era): one new level fires, two do not, and pausing
between them does not help. Every daemon write covers itself by nudging the
path it wrote (`Watch.writeSpans`); nothing covers a create by another
process. `AGENTS.hs:2074` states it: *"KNOWN GAP: an EXTERNAL create into a
fresh shard is invisible until a restart."* `[Unguarded]`. Directory events
are dropped by the `.org`-only predicate (`Watch.hs:82`), there is no
periodic walk and no rescan route (`Routes.hs:137-152`); the one full
re-walk is a config-path arriving in the queue (`Watch.hs:75-78`).

Every later Emacs edit to that row is lost the same way, for the daemon's
life — the CHANGELOG's own words.

## The contract nobody reads

org-glance's WAL, `.org-glance/meta/headlines.jsonl` (+ sealed `seg-*.jsonl`
under `MANIFEST`), is append-only, `seq`-monotone, one JSON record per
headline revision with `id`, `state`, `tags`, `schedule`, `archived`, … —
and it is written AFTER the blob (`org-glance-graph:add`: metadata, then
`put-content`, then `insert`). glance already parses it (`Data.Org.Index`,
the `wal=` side of `glance scan`'s drift report) — only once, in the CLI,
never in the daemon. It is the exact mirror of `meta/EXTERNAL.jsonl`, which
glance appends and org-glance polls.

## The fix

`docs/proposals/proposed/2026-08-26-the-daemon-tails-the-wal.md`: the daemon
tails the open segment and nudges each appended record's blob path through
the same door its own writes use.
