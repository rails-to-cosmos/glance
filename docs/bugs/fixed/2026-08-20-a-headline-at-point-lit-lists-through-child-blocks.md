# Bug — a headline at point lit lists through child blocks

**Status:** fixed 2026-08-20 · **Reported:** 2026-08-20, by the user
· **Surface:** the materialized doc pane

## Symptom

With point on a headline, the pane lit the top run of every list in the
subtree — child headlines' blocks included — while paragraphs, drawers and
every other element in those child blocks kept their resting bars. Lists
alone claimed a depth the rest of the picture never drew.

## Steps to reproduce

Materialize an entry holding a list on its own shelf and a child headline
that owns a list (the `drv-marks` fixture is that shape). Stand on the
entry's headline: the child's list bar wore the mark ink beside a resting
child paragraph.

## Evidence

The list bars spend `--ink` (`Style.hs`, `.d-list .d-item::before`), and the
headline-at-point rule set it with a descendant combinator —
`#mdoc.on .de.dat.d-head + .blk .d-comp > .de` — which reaches composites in
nested child blocks, while the shelf-stop twin on the same rule already used
the child combinator (`.blk:has(> .de.dat) > .d-comp > .de`). The comment
above the rule promises "ONE LEVEL DEEP"; the selectors under-delivered it.
A child's contents are a nested `.blk` of their own (`Doc.elm`, `blkOf`), so
the descendant form crossed shelves.

## Fix

The two headline selectors take the child combinator: `+ .blk > .d-comp >
.de`. Pinned in the browser case "a child is drawn whole, walked like a
list, and edits through the same splice": on the entry's own headline the
shelf's run bars in the mark and the child's list keeps its resting bar —
asserted red under the descendant form before the fix landed.
