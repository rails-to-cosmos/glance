---
name: decomment
description: Compact comments, docstrings and code-adjacent prose repo-wide — remove what the code already says, keep rules, invariants, directives and corner cases. Trigger on "decomment", "compact comments", "comment cleanup".
---

# Decomment — the code says it, so the comment goes

One law: a comment survives only by saying what the code cannot. Everything
else is noise the next edit turns into a lie.

## The classification

**KEEP, verbatim or tightened:**
- Invariants and laws — the ALL-CAPS-lead comments ("ONE EQUATION PER
  CONSTRUCTOR…", "SEEN GUARDS THE SIGN…"). These are the house register for
  a rule the code cannot show; they are the point of this skill, never its
  target.
- Directives — short advice to the next editor ("redirect to a log and echo
  $status", "run make elm before the suite").
- Corner cases — why a branch exists for an input nobody would guess
  ("a lone `-` negates the empty term and empties the table").
- Why-it-cannot-be-otherwise — constraints invisible in the code (locking
  order, wire compatibility, a peer's own rule).

**REMOVE:**
- What-the-next-line-does narration; restatements of a name or type;
  change-history ("now uses X", "was Y before"); reviewer-directed
  justification; section banners that describe rather than rule.
- A comment whose content became false or redundant after the code moved.

**COMPACT:** multi-sentence keepers become one law sentence where nothing
normative is lost. Docstrings stay checkdoc-valid — a complete imperative
first line, arg names in CAPS, facts intact; cut the bloat, keep every fact.

When in doubt: if deleting the comment loses no rule, no warning, and no
"why", delete it. Self-descriptive code is the default; a kept comment is a
claim that the code cannot speak for itself here.

## The hazards — check before every deletion

1. **Pinned comments.** TestServe and the browser suite quote source lines —
   comments included — verbatim (glue pins around `test/TestServe.hs`'s
   `Glue` cases; AGENTS.hs cites comment lines by content). Before removing
   or rewording ANY comment, grep its distinctive words across `test/` and
   `AGENTS.hs`. A quoted comment is load-bearing: keep it, or move the pin
   in the same change with the law named.
2. **Spec prose is not commentary.** `AGENTS.hs` Note strings ARE the spec
   (tier three of the model); `docs/invariants.md` entries are the rulebook;
   proposal files, `docs/bugs/`, `docs/agents/` and spike READMEs are argued
   records. None of these are targets. Inside AGENTS.hs only the plain `--`
   narration between constructs is fair game, and conservatively.
3. **Vendored and generated files are untouchable**: `assets/table-view.js`
   (synced from `../table-view` — editing it forks the sibling),
   `assets/elm.js` (compiled), `vendored/`, `dist-newstyle*`.
4. **User docs** (`docs/*.md` pages like query.md) are law pages, not code
   comments — compact only pure repetition, never a law's statement, and
   only when asked to include docs.

## The sweep

Work area by area (src-web, src-query, src, app, frontend/glue,
frontend/elm/src, test, tools). Per area: classify every comment, apply,
then prove nothing broke — the area's own gate first (`cabal build`,
`make check-glue`, `make elm`), the full battery once at the end
(`cabal test`, `runghc AGENTS.hs`, `make elm-test`, the browser suite).
A removed comment that was pinned shows up red — that is the safety net
working; restore or move the pin deliberately, never silently.

Report per area: comments removed / compacted / kept-as-law, and every
pinned comment encountered.
