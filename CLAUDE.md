The spec is [AGENTS.hs](AGENTS.hs) — glance's domain as a model. `runghc AGENTS.hs` checks it.

A bug is filed as its own file under `docs/bugs/`: the symptom, steps to reproduce, and the evidence as `file:line`. One bug, one file, the way `docs/proposals/` holds one design per file.

The rules a change must not break are in [docs/invariants.md](docs/invariants.md), each with its evidence and what breaks. Read it before touching the write path, the store or the walk.
