# Glance — Architecture Overview

## What it is

A **stateful org-mode parser** in Haskell — it parses Emacs org-mode text into structured data while accumulating context (TODO keywords, categories, headline identities) across parses. The long-term vision is treating org files as a **queryable, graph-structured database**.

## Architecture

### Two parallel module hierarchies (mid-refactor)

1. **`src/Data/Org.hs`** (monolithic, ~815 lines) — the current working code. Everything lives here: types, parser, context, timestamps. Uses a single-param `Parse` typeclass with `StateT Context (Parsec Void Text)`.

2. **`src/Data/Org/{Base,Context,Timestamp}.hs`** — a factored-out version in progress. `Base.hs` introduces a multi-param `Parse s a | a -> s` typeclass with a functional dependency, making the state type explicit per parsed type. `Timestamp.hs` is the most complete module in this new hierarchy. `OrgElement.hs` is entirely commented out — a graveyard of the old ADT-based `Element` approach before the existential type design.

### Supporting modules

- **`Persist.Org`** — TH-generated SQLite schema via `persistent` for `PersistentHeadline`. Currently just exports `migrateAll`; DB operations are commented out in the REPL.
- **`Repl.Org`** — Haskeline REPL that feeds text into `orgParse`, shows three representations (Show, TextShow, Display), and accumulates `Context` across inputs.
- **`Data.Config`** — Thin wrapper around Haskeline settings.
- **`Main.hs`** — No args = REPL; file arg = parse file then REPL.

## Key design decisions

- **Existential `Element`** wraps any type satisfying `(Show, TextShow, Typeable, Eq, Parse, Identity, Display)` — enables heterogeneous lists of Headlines, Pragmas, Timestamps, Tokens.
- **Content-addressable storage** via SHA256 hashing of headline titles (`HashID`).
- **Identity-addressable storage (IAS)** — headlines with an `ORG_GLANCE_ID` property get registered in a `Map HeadlineID Headline` inside `Context`, using last-writer-wins for collisions.
- **Typed references (`RefKind`)** — hierarchy, sibling, blocking, knowledge-graph edges between headlines. Defined but not yet wired into parsing.
- **`Headline` Semigroup** resolves conflicts by keeping the one with the later schedule — a merge strategy for the same headline seen across multiple parses.

## Org-mode features supported

- Multi-level headlines (`*`, `**`, `***`, ...)
- TODO/DONE states with dynamic keyword registration via `#+TODO:` pragma
- Priorities (`[#A]`, `[#B]`, `[#C]`)
- Tags (`:tag1:tag2:`)
- `:PROPERTIES: ... :END:` blocks
- Active `<YYYY-MM-DD>` and inactive `[YYYY-MM-DD]` timestamps with repeater intervals
- `#+CATEGORY:` pragma

## Test status

Only `TestLexer` (basic tokenization) is active. `TestParser` has thorough cases but is commented out in `Spec.hs` — likely needs updating after the existential `Element` refactor.

## What's incomplete

- The `Base.hs`/`Context.hs`/`Timestamp.hs` refactor is in-flight — not yet integrated
- `Ref` parsing not implemented
- Schedule/deadline parsing commented out in `Headline`
- DB CRUD operations not yet built
- Link, comment, and table parsing not started
