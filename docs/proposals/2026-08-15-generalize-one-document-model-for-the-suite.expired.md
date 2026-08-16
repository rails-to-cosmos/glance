# Proposal — the suite already has a document model, and one file uses it

**Status:** expired — EXPIRED 2026-08-15, on the first attempt to implement it ·
**Date:** 2026-08-15 · **Origin:** `/generalizer`, the cross-cut and
future-variant angles.

## What was claimed

`test/TestGen.hs` (622 lines) models an org document precisely — `DocSpec`,
`EntrySpec`, `render` emitting the text *and its expected spans* — and only
`test/TestProperties.hs` imports it. The other fifteen test files spell org by
hand as string literals, **1,271 lines** of them. The proposal was to promote
`TestGen` to the suite's fixture vocabulary: add `docText :: DocSpec -> Text`
and constructors (`doc`, `entry`, `todo`, `tagged`, `planned`, `propped`,
`bodied`), then convert file by file, largest first, for a claimed **−400 to
−600 lines**.

## Why it expired

The proposal named its own failure condition — *"the example tests turn out to
depend on byte-level details `DocSpec` abstracts away … sample twenty
conversions in `TestQuery.hs` before touching the rest"*. The sample was run.
It passed, and three measurements taken beside it killed the proposal anyway.

**1,271 lines is coverage, not repetition.** Across all fifteen files there are
**102 distinct fixtures and 9 redundant copies** — six literals appearing more
than once, five of them inside `TestQuery.hs` alone, one shared between
`TestServe.hs` and `TestSpec.hs`. There is no copy-paste body to absorb. Every
other fixture is a different document pinning a different case, which is the
suite doing its job. The original measurement counted VOLUME and read it as
DUPLICATION.

**The constructor form is longer than the literal it replaces.** `TestQuery.hs`'s
73 fixtures split **36 of ≤3 org lines (82 lines) against 37 of ≥4 (210)**, and
the short half dominates the call sites. `doc` and `planned` also collide with
existing top-level bindings in `TestQuery.hs`, forcing a qualified import, so:

```haskell
(T.unlines ["* TODO Bare", "body line"])                              -- 44 chars
Gen.docText (Gen.doc [Gen.bodied ["body line"]
                       (Gen.todo "TODO" (Gen.entry 1 "Bare"))])       -- 85 chars
```

Even the drawer case, where one constructor replaces three literal lines, comes
out level or worse. The LOC claim was wrong in sign for the common shape.

**The strongest-looking family is byte fixtures by design.** "Ship it" appears
101 times in `TestQuery.hs` and looked like the parameterize case. It is
input/expected PAIRS for edit operations — `setStateIs`, `archiveIs`,
`addTagIs`, `removeTagIs`, `renameTagIs` — where the point IS the bytes.
`TestQuery.hs:1401` pins `"*   NEXT   Ship it\n"` with its irregular internal
spacing, which `esTitle :: [Text]` cannot express at all. Converting these would
delete the assertion.

## What was verified, and is worth keeping on the record

`docText` and the seven constructors were written and checked against thirteen
real fixtures drawn from `TestQuery.hs` — bare-plus-body, tags with a child, a
property drawer, a `#+TODO:` cycle line, a unicode title, a multi-word title,
priorities, four-level nesting, and the blank entry `"* "` with its trailing
space. **All thirteen reproduced byte-for-byte.** The abstraction is correct and
`TestGen` can spell what the example tests spell. What is missing is a reason:
correct, and unneeded.

The code was reverted; `test/TestGen.hs` is unchanged from `eaf1f27`.

## What would revive it

A test file that needs fixtures **with their expected spans** — the one thing a
literal cannot carry. `TestSpans.hs` (50 hand-spelled org lines) is the only
current candidate, and it is small enough that the import costs more than it
saves. Should a second span-asserting suite appear, start there rather than at
the largest file.

## The lesson for the next sweep

Volume of fixture text is not a generalization target. **Measure repetition —
distinct values against total occurrences — before proposing an abstraction over
a body of literals.** `2026-08-15-generalize-cli-commands.proposed.md` and
`2026-08-15-generalize-keymap-dialects.proposed.md`, written in the same sweep,
name a repeated SPELLING of one fact rather than a volume of distinct ones, and
that is the distinction that decides.
