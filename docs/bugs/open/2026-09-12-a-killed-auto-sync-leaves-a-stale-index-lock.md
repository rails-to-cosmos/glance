# Bug — a killed auto-sync leaves a stale `index.lock`, and every later git call fails

**Status:** open · **Reported:** 2026-09-12 (`~/sync/views`) · **Surface:**
`Git.hs` auto-sync (Model B), `GET /git`, any other git user of the store

## Symptom

```
128 git … add -- .org-glance/data/cd/5bbe93-bd6e-492d-8fb7-d56475a32a28/
fatal: Unable to create '/home/akatovda/sync/views/.git/index.lock': File exists.
```

`.git/index.lock` is an empty file dated 15:10:27; no git process is alive;
`glance.autosync` reads `false`. The reflog shows `glance: sync` commits up to
13:25:53 and nothing at 15:10, so a git step started at 15:10 and died
holding the lock. From then on every git call in the store fails with 128,
including callers outside glance (the `add -- <shard>/` above is not glance's
spelling; glance runs `add -A`).

## Steps to reproduce

1. `git config glance.autosync true`, arm it, make a write so the worker
   runs `add -A` / `commit` / `push`.
2. Kill the daemon (or the machine sleeps) while a git step is mid-flight.
3. Restart. `git status` from anywhere in the store: `index.lock: File exists`.
   `GET /git` answers as if the tree were readable; the readout says nothing.

## Evidence

- The worker shells git with `readGit` and checks only the exit code
  (`src-web/Glance/Web/Git.hs:136-140`); no bracket, no mask, nothing
  cleans up a child killed by the daemon's own shutdown.
- `gitStatus` reads `rev-parse` then `status --porcelain=v2`
  (`Git.hs:84-88`); a lock makes `status` fail, and the failure is not a
  state the readout draws (`glyphFor`, `frontend/glue/80-git.js`).
- Nothing in glance or the peer inspects `index.lock`.

## Proposed fix

- **Never leave the lock.** Run each git step under `bracket`/`finally` so a
  daemon shutdown waits for the child (or terminates it and lets git clean
  its own lock); `withAutoSync`'s stop path joins the worker before exit.
- **Say it.** `gitStatus` gains a `locked: <mtime>` reading when `index.lock`
  exists and no `git` process holds it (age above a few seconds); the
  readout's hover names it (`index.lock stale since 15:10`) and the glyph
  wears the warn dress. Never delete the lock from glance: the reader does,
  once, by hand.
- **Test.** TestGit: a fixture repo with a planted `index.lock` reads as
  locked; the worker's stop path is exercised with a slow fake `git` on PATH
  and leaves no lock behind.

## Not this bug

Who ran `git add -- <shard>/` and the fetch at 16:55 is outside glance and
the peer (neither shells a per-shard add); it only met the lock.
