# Design rhymes

**Status:** doctrine · **Date:** 2026-08-03

Nothing new invents an interaction: it rhymes with one that already exists.
A reader who learns one surface has learned them all — the rhymes are why the
habits form.  Before a feature gets built, it answers one question first:
**what does this rhyme with?**  A feature that can't answer is either the
first of a new family (rare — name the family and its rule) or wrong.

## The ledger

**Movement.**  The table's letters are THE movement vocabulary, and they are
the editors' own: Emacs's `n`/`p` (next/previous) and `f`/`b`
(forward/backward) beside vim's `j`/`k` and `l`/`h` — and the arrow keys,
the classic every hand knows, ride along on both axes.  All three dialects
bound everywhere at once: a hand trained anywhere lands here already knowing
the keys.  The property panel, the link and tag popups, and the material
document all speak it.  Vertical wears the row wash; horizontal wears the
crosshair's column hue — in the document as in the table.

**Backwards is one gesture.**  `DEL` is the backspace key wearing its oldest
meaning — erase the last thing you did — lifted from characters to
structure: filter tokens, then filter frames (the drill stack), sort tokens,
a child sheet up to its parent, the sheet closed at the top.  In a text
field it still erases characters; over structure it erases steps.

**Deletion is one gesture.**  dired's flag-then-confirm everywhere something
dies: `d` flags red, `d` on a flagged thing (or `D`) takes every flagged
thing, `u` unflags first.  Rows archive, properties and paragraphs delete,
planning clears — same keys, same wash, same repeat-guard, one `flagKey`.

**Editing is one overlay.**  `RET` opens the thing at point in place —
value-first fields over their own cells, `TAB` hops, `RET` commits, `ESC`
restores.  Property rows, tag renames, link edits, paragraph blocks: one
`openEdit`, shapes per surface.

**Letters commit only where one press is one act.**  The which-key state
palette is the family's sole member: a letter both chooses and commits, so
the palette is the confirmation.  Anything browse-and-act is a mount
(links, tags); anything type-and-commit is a text field.  `/` always narrows.

**Stars mean meta.**  `*active*`, `*inactive*`, `*empty*`, `*archive*`,
`*none*` — a starred word is reserved semantics in every context, never a
literal; matching reads through the stars (completion is star-blind), the
walls (`keywordTextP`, `isTagChar`) make a starred literal undeclarable.

**Decoration for eyes, substance for predicates.**  `[#A]` displays; the
predicate folds the brackets.  Links underline in `--tv-link`; matching never
cares.  The weekday renders computed; the parser discards what it read.

**The grammar is one language.**  Predicates narrow, sort tokens order,
alternatives OR inside a token, tokens AND across, written order is
precedence, repeats collapse at the door.  Filter and sort share the box,
the URL, the chips, `DEL`, completion and the crumbs.

**One list, many readers.**  `viewColumns`, `keywordScopes`, `SURFACES`,
the commands table, the route table: the order is spelled once and every
consumer reads it.  When prose says "kept in sync", the design owes a
derivation.

**Emacs is the source of the spellings.**  Keys and command names are
org-glance's/org's own (`^`, `:` = `org-agenda-set-tags`, `C-c '`,
`C-x C-s`); the web replaces Emacs *mechanisms* (prefix arguments become
ordered presses with visible chips; `org-hide-leading-stars` becomes the
document's star prefix) but never renames the muscle memory.

**Honesty chrome.**  Echoes speak command identifiers verbatim; the log is
append-only and titled; refusals name what refused; empty parts render
nothing — the absence is the display.

## The test

A proposed feature states its rhyme in one sentence ("child sheets pop with
DEL — the drill stack's rule").  Review asks whether the rhyme is real: same
keys, same wash, same refusal shape, no second implementation where a
parameter would do.  The coherence sweeps exist to catch the rhymes that
drifted.
