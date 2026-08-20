# Bug — interop's seed dies on a peer API that never landed

**Status:** open · **Reported:** 2026-08-20 · **Surface:** `make interop`
· **Cross-repo:** the peer is `../org-glance` (`OG_HOME`)

## Symptom

`make interop` fails before the first of the 13 cases runs:

```
interop: the driver itself failed — Error: emacs seed exited 1
{"error":"(void-function org-glance-graph:external-path)"}
```

Exit 2, 0/13. Reproduced twice on 2026-08-20 (the additive-filters gate run
and a hand run). The additive-filters change is out of the blast radius: the
seed step runs before the glance daemon is spawned
(`test/interop/drive.mjs:231` vs `:236`), so no glance code executes at all,
and `git diff HEAD -- test/interop/` is empty.

## Evidence

- `test/interop/og.el:146` calls `org-glance-graph:external-path` in the seed
  step, unguarded; `:117`, `:155` and `:164` call it again. Only
  `--external-folded` (`og.el:116`) and `--external-pending-p` (`og.el:126`)
  are `fboundp`-guarded.
- The peer checkout (`../org-glance`, HEAD `6798490` "capture relations",
  2026-07-26) has no `org-glance-graph:external-path` in any `.el` — the
  colon-accessor family in `src/data/org-glance-graph.el` (30 `cl-defun`s,
  `:549`–`:1052`) stops short of it, and the whole external-notification API
  og.el contracts against (`--external-tail`, `--external-sources`,
  `--read-external`) is absent too. `git log -S "external-path" --all` there
  finds nothing: the API has never been committed to the peer.
- `og.el` landed asserting that API on 2026-08-13 (`f4f7710`, `618c8b4`), two
  weeks after the peer's last commit. The peer's reflog shows three
  `reset: moving to HEAD` entries on 2026-07-25 — the API most likely lived
  in the peer's working tree, uncommitted, and a reset took it back.
- The handoff of 2026-08-20 records `make interop` 13/13 at `9d40fda`; that
  tally is unreproducible against the peer as checked out today.

## Resolution owed

A two-repo decision: either the external-notification API is (re)committed to
org-glance, or `og.el` re-pins its contract to what the peer provides — the
bare `org-glance-graph:external-path` calls gaining the `fboundp` guard the
two `--external-*` probes already have, with the external cases skipping
loudly the way the harness skips a missing checkout.
