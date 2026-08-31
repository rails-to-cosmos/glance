# Elm code style

Conventions the `frontend/elm/` sources hold to. No formatter runs in the build,
so the convention lives here: it keeps `elm-format`'s shape by hand and departs
from it only where the extra air costs more than it gives.

- **A short `if … then … else` stays on ONE line** when the whole
  `if C then A else B` fits (~100 columns) and each branch is one simple
  expression — a literal, a variable, one application:
  `if inList m then "focus" else ""`.
- **A multi-line `if` carries NO blank line before `else`.** `elm-format` spaces
  them out; here the branches sit tight, so a two-way choice reads as one shape,
  not three.
- **Expand only when a branch is itself a block** — a `let`, a `case`, a nested
  `if`, a multi-line record or list — or when the if is a broken-out element of
  an already multi-line tuple.
- **A short `let` binding stays on ONE line** — `name = expr` when it fits
  (~100 columns) and the RHS is one simple expression (a literal, a variable, one
  application): `here = List.length out`. Expand when the RHS is itself a block —
  a `let`, `case`, `if`, multi-line record or list.
- **`let` bindings sit tight** — NO blank line between them. `elm-format` spaces
  them; here they read as one block, the way tight `if` branches do.
- **A short `case` arm stays on ONE line** — `pattern -> expr` when it fits
  (~100 columns) and the body is one simple expression; the arms then sit tight,
  no blank line between them. Expand an arm whose body is a block — a `let`,
  `case`, nested `if`, multi-line record or list.
- **A short top-level definition stays on ONE line** — `name args = expr` when
  it fits (~100 columns) and the body is one simple expression; expand when the
  body is a block (a `let`, `case`, `if`, multi-line record/list/pipe).
- Otherwise follow `elm-format`: one blank line between top-level declarations.

Canonical shape:

```elm
mark =
    if k <= 0 then
        []
    else
        [ span [ class "dm" ] (markParts op head) ]

word =
    if by > 0 then "next-row" else "previous-row"
```
