# Proposal — every region says what a new line inside it looks like

**Status:** done — built 2026-08-12, the nesting closed 2026-08-13, the two
walks made one and the depth settled on org's greater/lesser split the same
day ·
**Date:** 2026-08-12 · **Origin:** two bugs reported against the running
sheet — `S-RET` inside a real checkbox list yielding an item with no lead, and
a caret on a non-item line splicing a new item into the middle of a
`#+begin_src` block.

## The gap

`Scan.itemLead` is the LIST's rule, and everything else falls through it.
That has three consequences, all of them live:

1. **A caret on a line that opens no item corrupts the structure it stands
   in.** `leadLine` falls back to the stop's first line for the LEAD, and
   `anchored` still anchors on the raw caret line, so the two disagree:

   ```org
   - alpha
     #+begin_src sh
     echo hi
   - NEW               <- the block is cut in half
     echo bye
     #+end_src
   ```

   Reachable from the sheet, because the scanner gives such an item ONE stop:
   the box holds every line of it, so the reader's only caret is inside them.

2. **A block and a table have no answer at all.** They keep the composite's
   landing, which CLAUDE.md justifies as *"a pipe row and a source line being
   no prefix the page can spell — and grown in place org would cut the table
   or take the prose for source."*

3. **Nothing knows which region a line belongs to.** Each rule re-derives it,
   which is why a checkbox list read as no list at all
   (`views/.org-glance/data/47/35e269-.../data.org`) has no single place to be
   fixed.

## The design

ONE QUESTION — which region holds the caret. ONE ANSWER PER REGION — its
continuation.

```elm
type Kind = Plain | Item | Table | Block | Drawer

type alias Region =
    { kind   : Kind
    , from   : Int      -- first line
    , to     : Int      -- last line; the closer where the kind has one
    , indent : String   -- what a continuation wears
    , marker : String   -- what a new line inside it opens with
    }
```

`itemLead` becomes `markerFor : Region -> String` — one rule per kind.

| region | marker | why that one |
|---|---|---|
| bullet item | its own bullet | org's `M-RET` |
| checkbox item | bullet + `[ ]`, the box EMPTY | a new task is undone |
| numbered item | the next number | org's own; a duplicate is `org-list-repair`'s job |
| **table** | **an empty row, columns aligned to the table's own widths** | a blank line ENDS a table |
| `#+begin_X` block | empty, at the block's indent | source is not org markup |
| drawer, `:LOGBOOK:` included | empty, at the drawer's indent | a drawer holds lines |
| paragraph | empty, with a blank line above | today's rule, unchanged |

**The table's marker reproduces org's alignment** — `|`, then each column
padded to its current width, `|`-separated, measured off the table's existing
rows with `|---+---|` rules excluded from the measurement:

```org
| alpha | beta  |
|-------+-------|
| one   | two   |
|       |       |   <- the new row
```

**THE ANCHOR.** Immediately after the caret's line, INSIDE the region — except
on a region's CLOSING line (`#+end_X`, `:END:`), where it lands after the
region. A table has no closer, so a caret on its last row keeps the new row
inside it, which is how a table is actually built.

**ABSENT CARET IS UNCHANGED.** `+` with no open edit supplies no index, and
that still rides past the whole structure. The caret is what makes a region's
interior addressable; without one there is nothing to be inside.

## What this reverses, deliberately

CLAUDE.md today says a `#+begin_X` run and a table keep the composite's landing
because a source line and a pipe row are *"no prefix the page can spell"*. This
withdraws that claim: an empty line is a prefix anything can spell, and a table
row is one the SCANNER can spell from the table it already parsed. The page
still spells none of it — `Scan.elm` owns every marker and the shell sends a
line index, which is the invariant that matters and is untouched.

The caution the old rule carried was real and is answered rather than dropped:
*grown in place, org would cut the table or take the prose for source.* Cutting
is exactly what the corruption bug does today, and the region rule is what
stops it — the new line lands inside the region wearing the region's own
continuation, so the block still closes and the table still parses.

## Tests owed — written

In `assets/elm/tests/ScanTest.elm`, and the first two are the reported bugs:

- the reported checkbox list is read as a list, and `S-RET` in it yields an
  item wearing the caret line's own prefix;
- a caret inside a `#+begin_src` run adds an EMPTY line inside the block, and
  the block still closes;
- the same for a drawer, `:LOGBOOK:` included;
- a table gains an aligned empty row, and the table still parses;
- a caret on a closer (`#+end_X`, `:END:`) lands AFTER the region;
- a caret on a table's last row lands INSIDE it;
- a numbered list continues from the caret's own number;
- a checkbox comes back EMPTY whatever the caret line's state;
- an ABSENT index still rides past the whole structure;
- and EVERY KIND GETS ITS NESTED TWIN — a block, a table and a drawer riding
  inside a list item, plus a caret on the nested closer. The first 108 cases
  tested each kind at TOP LEVEL alone, which is how the walk settling on the
  item survived them;
- and a BULLET INSIDE each nested kind, asked BOTH ways — the stops it yields
  and the body every `d`/`D` over them leaves. Three rounds of green suites
  missed the scanner's own blindness because no fixture put a bullet inside a
  nested block or drawer, so these do: a bullet in a `#+begin_src` and in a
  `:LOGBOOK:` under an item, a pipe row inside a block at both depths, a block
  inside a drawer at both depths, a bullet inside a TOP-LEVEL block, the blank
  org keeps inside a nested run, and the five-kind top-level stop set pinned
  whole.

And one that is not a case but a discipline: the rule must not WIDEN what
counts as a list, a table or a block. Text org would decline — a bullet with no
space after it, a `#+begin_` with no matching `#+end_`, a pipe row after a
blank line — must still be declined.

## As built

`Scan.regionAt` is the walk, `Scan.markerFor` the answer per kind, and
`Scan.Region` is the record above with the marker DERIVED rather than stored —
a table's is measured off the rows it already spells. Drawers were the new
work: they are REGIONS and no stops, so `blocksIn` draws what it always drew
and recognizing one buys where a line inside it goes.

**THE WALK RE-ENTERS THE ITEM IT SETTLED ON** (`withinItem`), which the first
cut did not: an item's lines are a body of their own, so the same question is
asked again between its first line and its last. Without that, the spec's own
motivating example still corrupted — a caret in a `#+begin_src` riding under
`- alpha` was answered with `- `. What no nested region claims is the ITEM's,
and so is the CLOSING line of one, which asks for what comes after it.

**AND THE STRUCTURE SCANNER READS THE SAME WALK** (`Scan.regionsIn`), which the
second cut did not. This section said the scanner "needs no matching recursion",
and that sentence was the third round of one bug: `blocksIn`'s `pushItem` hunted
an item's raw lines for `listOpener` knowing nothing of `blockName` or
`drawerName`, so a bullet inside a nested block was minted as a STOP running to
the block's own closer — the pane drew it, `f` descended onto it, and `d` then
`D` took the `#+end_src` out and left the block unclosed. THE TWO WALKS THAT
MUST AGREE ABOUT REGIONS ARE NOW ONE: `regionsIn` answers which regions sit at a
level, `regionAt` picks the one holding a line, and `pushItem` mints a stop per
ITEM the walk names. A region need not be a stop — a nested one is the item's
own lines, stops there moving the grain rather than saying where a line goes.

Two rules fall out of the join. ITEMS TILE the run they sit in, org keeping one
blank line inside a list, so no line inside a list is nobody's; a stop cut from
one is `snug`, which is what keeps a nested item's span the one `listRun` spells
at the top level. And THE TOP LEVEL READS THE SAME `kindAt`, with ONE
arm of its own: `Drawer` sends the line to prose, so a drawer holding a block is
three top-level stops and taking the first leaves `:END:` standing. That is
open, pinned by its own cases — making that arm a stop would re-cut the pane
over every drawer in the corpus.

**AND POINT LANDS WHERE THE READER TYPES** (`Scan.caretIn`, riding the state
push the marker does): at the end of a lead, and INSIDE THE FIRST CELL of a
table row, since typing past its closing pipe opens a column org's align keeps.

**HOW DEEP THE WALK GOES IS ORG'S OWN GREATER/LESSER SPLIT** (`Scan.greater`),
which is the fourth and last cut and the one that stopped this file inventing a
rule per kind. Re-entering the ITEM alone left two holes and one sentence closes
those two: *the walk re-enters a GREATER region and treats a LESSER one as
opaque.* It closes the two below and nothing else — the top-level drawer under
[Open](#open) is a third, and it is the top level, which never asks this
question.

A greater element CONTAINS elements — an item, a drawer, and every block org
parses the contents of (`center`, `quote`, and any SPECIAL block a tree names
itself). A lesser element holds none: the five VERBATIM blocks, the names
`org-element-greater-elements` leaves out (`comment`, `example`, `export`,
`src`, `verse`). Org suspends its grammar inside those five, which is exactly
why an empty line is the right continuation there.

THE VARIABLE IS THE ELEMENT ONE and the count is why. `org-list-forbidden-blocks`
is org's LIST rule, names four, and spares `comment`; read here as though it
answered about every kind, it left `#+begin_comment` holding items and tables.
Asked of org rather than of the code — a table, a list, a drawer and a block put
inside 21 block names, then `org-element` asked what it parsed — the unparsed
five are `src`, `example`, `export`, `verse` and `comment`. Zero occurrences in
this corpus, and the reach is the MARKER alone: no stop is minted inside a block
and `listRun` steps over a closed run already.

AND A TABLE IS GREATER IN ORG. It is in `org-element-greater-elements`, since it
contains `table-row`. This walk treats it as a leaf, which is the one name where
the two lists differ and costs nothing: a table's only child is a row, and a row
is what the Table marker spells.

- **Reachable on the corpus.** `views/.org-glance/data/gy/m-25044-…/data.org`
  is a `#+begin_pin` holding a 21-row table — a special block, so its contents
  are elements. Answered with the item-only walk, a caret anywhere in those rows
  took the BLOCK's empty line and org's verdict was
  `[1 table, 21 rows] -> [2 tables, 21 rows]`. Six files in this corpus carry
  the shape; `src`/`example`/`export`/`verse` were immune by accident, being the
  kinds the old rule happened to name.
- **And latent one layer down.** `listRun` hunted `listOpener` through a block
  it knew nothing about, so an item boundary could be cut THROUGH one: a `- b`
  between two source lines ENDED the item above, and taking that item carried a
  `#+begin_src` off without its `#+end_src`. `listRun` steps over a block or a
  drawer WHOLE now, which is org's own `org-list-struct` in its own words —
  *"skip block or drawer at point, and move to next line"*. Zero occurrences in
  6331 corpus files, and present at HEAD, so it was latent rather than a
  regression.

Two answers move with the rule and both are org's. A block inside a nested
DRAWER is now the BLOCK's lines rather than the drawer's, and a bullet inside a
`:LOGBOOK:` is an ITEM — which is how the corpus's own state lines
(`- State "DONE" from "TODO" …`) read.

**THE FIXTURES PAIR EVERY CONTAINER WITH EVERY KIND**, at top level and inside
an item, each asked both ways — the stops it yields, the body every `d`/`D`
composes, and the write at EVERY caret. What was missing was exactly where the
defects lived: drawer-contains-table, greater-block-contains-table,
block-contains-drawer, and a block or drawer straddling an item boundary. The
pipe-row-in-a-block fixtures used `#+begin_src org`, the one kind where a pipe
row CANNOT be a table, so the pairing was asserted only where it could not fail;
they keep that verbatim assertion under their own name and `quoteGrid` is the
same pairing where it can. THE ORACLES SAY ONLY WHAT THEY CHECK: `pairsUp` is a
line count over openers and closers and `tableRuns` a second line reading beside
it, and neither is org's parser.

## Open

- **A COLUMN-1 DRAWER IS NO STOP, and the greater/lesser rule does not reach
  it.** `blocksIn`'s top level recognizes lists, tables and blocks and not
  drawers, so a `:LOGBOOK:` at column 1 and its `:END:` are ordinary paragraph
  lines there and a block inside one splits them into three stops. `d` on
  either leaves a stray `:END:` or an unclosed drawer. Measured: 2 bad writes
  out of 28,431 corpus deletions, in ONE file (`data/gy/m-25008-…/data.org`, a
  `:TIMESTAMPS:` drawer of three bullets); 3 of 12,206 bodies carry a column-1
  drawer at all. `ScanTest` pins it as the named asymmetry. Closing it means
  the top level reading the walk, which re-cuts the pane over every drawer in
  the corpus.
- **A CONTINUATION LINE ANSWERS WITH ITS ITEM'S BULLET.** A caret on an item's
  own wrapped prose takes the item holding it — the rule that keeps a column-1
  bullet out of a nested run — so `S-RET` there cuts the paragraph and org
  reads one more item. 612 such writes over about four corpus documents. Org's
  own `M-RET` opens an item from a continuation line too, so which of the two
  this should be is a decision rather than a defect; `ScanTest` holds today's
  answer so a change names it.
- **`:LOGBOOK:` is a plain drawer here.** A clock-line marker would be guessing
  at what belongs in one. If a logbook wants its own, it is one row of the
  table above.
