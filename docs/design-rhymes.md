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
A letter is a PHYSICAL key, one rule in `keyName` that every listener
inherits: `т з о л` on a Cyrillic layout are `n p j k`, since the fingers
know where the letters sit rather than what the layout writes there.
Punctuation stays the character — it sits nowhere in particular.

**Movement never changes context.**  `n`/`p`, `f`/`b` and the grain keys only
relocate attention: they never open, never close, never commit, never cross a
boundary a reader would have to come back out of.  `RET` and `DEL` are the
context axis — `RET` goes deeper (opens the edit, enters the child, raises the
thing's own popup) and `DEL` comes back out (unmark, token, frame, the sheet
ladder, close).  A key that both moved and switched would make every press a
risk to weigh; the split is what makes holding `n` safe anywhere on the page.

**Movement is two axes, and the table teaches both.**  `n`/`p` walk the
COARSE axis and `f`/`b` the FINE one — rows and cells in the table, siblings
and grain in the document.  One habit serves both surfaces: in the material
document `n` steps composite-to-sibling (a list is one stop, so holding `n`
skims at reading grain), `f` goes a grain finer (a composite's leaves, a
headline's cells) and `b` a grain broader — `b` as in BROADER — with the
refusals spoken rather than swallowed.  The grain keys are still movement:
`b` at the floor is a no-op with an echo, never a close, because going out of
the sheet belongs to `DEL`.  (This replaced the earlier one-walk grain, where
`n` dived into every composite: elegant, but private to this pane, and the
skim is what reading wants.)

**Backwards is one gesture.**  `DEL` is the backspace key wearing its oldest
meaning — erase the last thing you did — lifted from characters to
structure: a marked set, then filter tokens, then filter frames (the drill
stack), sort tokens, a child sheet up to its parent, the sheet closed at the
top.  A popup with no inner ladder IS the last structure standing, so `DEL`
steps out of it: over the link and tag popups it closes them, where `ESC`
goes.  The state palette is the exception and keeps its own landed meaning
(`DEL` commits `*empty*`), a value being what that surface is for.
Grains are not a rung: the walk crosses them, so there is nothing there
for `DEL` to undo.  Marks go first, being the last structure a reader put there; flags stay,
being the archive queue rather than a mark.  In a text field it still erases
characters — an open rename or link edit inside a popup keeps the field's
own erase, and only nav-mode `DEL` closes — over structure it erases steps.
A field the reader SUMMONED is the one exception, and it rhymes rather than
breaks: emptied, the picker's inline filter box is itself the last structure
they put there, so `DEL` takes the box before it takes anything under it.

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

**The typed value is always an offer.**  Where a field's vocabulary is OPEN —
the add-a-tag palette, the kind picker, both halves of the sheet's pair box —
the line the reader typed is drawn as its own leading entry, hinted `new`, so
`RET` commits the word they spelled and a match is one `C-n` away.  Three
widgets, one rule: the hint word is `NEW_HINT` and the fold-equality test is
`leadTyped`, both spelled once in `frontend/glue/00-core.js`.  A typed value
that case-folds to an entry coincides with it — one entry drawn, never two,
and that entry leads, since the coincidence is asked of the whole vocabulary
rather than of what a cap left standing.  An empty field offers no literal.
AGENTS.hs carries the law; `docs/bugs/fixed/2026-08-22-an-open-completion-swallows-the-typed-value.md`
carries what it cost to learn.

**Stars mean meta.**  `*active*`, `*inactive*`, `*empty*`, `*archive*`,
`*none*`, `*today*` — a starred word is reserved semantics in every context,
never a literal; matching reads through the stars (completion is star-blind),
the walls (`keywordTextP`, `isTagChar`) make a starred literal undeclarable.

**Decoration for eyes, substance for predicates.**  `[#A]` displays; the
predicate folds the brackets.  Links underline in `--tv-link` — in the table's
cells and in the material document alike, where `[[T][D]]` shows `D` and `RET`
still opens the raw org; matching never cares.  The weekday renders computed;
the parser discards what it read.

**The grammar is one language.**  Predicates narrow, sort tokens order,
alternatives OR inside a token, tokens AND across, written order is
precedence, repeats collapse at the door.  Filter and sort share the box,
the URL, the chips, `DEL`, completion and the crumbs.

**One list, many readers.**  `viewColumns`, `keywordScopes`, `SURFACES`,
the commands table, the route table, the popup size tiers: the order is
spelled once and every consumer reads it.  A popup wears `.pop-band` or
`.pop-sheet` and declares no width of its own; a surface needing a third
measure adds a tier rather than a rule.  When prose says
"kept in sync", the design owes a derivation.

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
