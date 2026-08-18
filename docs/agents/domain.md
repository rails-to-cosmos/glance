# Domain Docs

How the engineering skills should consume this repo's domain documentation when
exploring the codebase.

## Before exploring, read these

- **`AGENTS.hs`** at the repo root — the domain model, written as Haskell rather
  than markdown. It replaced `CLAUDE.md` and `docs/invariants.md` on 2026-08-13.
  It carries the vocabulary as types, the registries as data, and the rules
  types cannot state as `Note` entries.
- **`docs/proposals/`** — this repo's decision records. Read the ones touching
  the area you are about to work in.

`CLAUDE.md` and `AGENTS.md` are one-line pointers to `AGENTS.hs`. That is
deliberate: one file, one copy of each rule.

There is no `CONTEXT.md`, no `CONTEXT-MAP.md` and no `docs/adr/`, and none
should be created — each would be a second copy of something above, and two
copies drift. That is the failure this repo keeps finding in its own documents.

## The model is checked, so read it as fact

- `runghc AGENTS.hs` prints the model and its notes, and exits non-zero on a
  contradiction.
- `test/TestSpec.hs` imports the model and asserts its registries against the
  REAL symbols, so a registry that drifts from the code fails `cabal test`.

A rule stated in `AGENTS.hs` is therefore load-bearing. Before
contradicting one, check whether a test already pins it.

## File structure

```
/
├── AGENTS.hs          ← the domain model; `runghc AGENTS.hs` checks it
├── AGENTS.md          ← one line, pointing at AGENTS.hs
├── CLAUDE.md          ← the same one line
├── docs/proposals/    ← decision records, YYYY-MM-DD-<slug>.<status>.md
└── test/TestSpec.hs   ← asserts AGENTS.hs's registries against real symbols
```

## Use the model's vocabulary

When your output names a domain concept — an issue title, a proposal, a
hypothesis, a test name — use the term as `AGENTS.hs` spells it: `Span`,
`RowId`, `Region`, `Digest`, `Cursor`, `Layer`, `Surface`. Its glossary is the
project's ubiquitous language, and the code is written in it.

If the concept you need has no type there, that is a signal: either you are
inventing language the project does not use (reconsider), or there is a real gap
worth adding to the model.

## Flag conflicts with a proposal or a Note

If your output contradicts a decision record or an `AGENTS.hs` `Note`, surface
it rather than silently overriding:

> _Contradicts `2026-08-12-region-markers.done.md` (a caret on a closing line
> lands past the region) — worth reopening because…_

A `Note` carries the `Proof` that would catch it going wrong. A Note marked
`[Unguarded]` is relied on silently and is the cheapest thing to turn into a
test.
