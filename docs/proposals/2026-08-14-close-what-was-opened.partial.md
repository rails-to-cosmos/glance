# Proposal — a commit closes what the typing opened

**Status:** partial — landed 2026-08-14; the corpus oracle is still owed ·
**Date:** 2026-08-14 · **Origin:** user — typing
`#+begin_src` and committing should leave a block, not a stray line.

## What it does

Commit an element whose text opens a block or a drawer and never closes it, and
the closer is written with it:

```org
#+begin_src            →     #+begin_src
                             #+end_src

:LOGBOOK:              →     :LOGBOOK:
                             :END:
```

Generalized: `#+begin_NAME` earns `#+end_NAME`, any drawer opener earns `:END:`.

## Where it belongs, and why not the server

**`assets/elm/src/Scan.elm`.** It already models every part of the question:

- `blockRun` and `drawerRun` return `-1` when no closer is found, and
  `Scan.elm:456` already turns that into `Nothing` — **the signal this feature
  needs exists today**;
- `verbatim` (`:631`) holds the five blocks whose contents org does not parse;
- the scanner already knows nesting, indentation and where a region ends.

The server is the wrong home, and this is the load-bearing reason: **Haskell has
no block model at all.** `#+begin_` appears nowhere in `Data.Org.Parser` — a
block is body text to it. Putting the completion there means teaching Haskell
what a block is and carrying a SECOND copy of the verbatim list, which the two
scanners could then disagree about. `AGENTS.hs` already carries that list as
model data; a third copy in real code is the drift this repo keeps finding.

**The cost, stated plainly:** the doc pane gets this and capture (`+`) and raw
mode (`C-c '`) do not. They are plain-text surfaces with no structure scanner,
and that is a deliberate limit rather than an oversight. If capture should
complete too, the honest move is to give it the scanner, not to duplicate the
rule.

## Step one — `Scan.elm` stops lying, by subtraction

The module is 1454 lines doing NINE things, and its name covers two. Its own
docstring already says so: "the structure a subtree's body has, the rows it
becomes, the splice that composes one back, and the readings a cursor is moved
by."

| half | sections | top-level functions |
|---|---|---|
| **what org text IS** | line predicates, what a line opens, the region walk, the structure scanner | 38 |
| **what the pane makes of it** | rows, the splice, the markers, the insert, cursor and grain | 39 |

Move the second half to `Body.elm` and `Scan.elm` keeps only what scans — the
name becomes true with nobody renaming anything. (`Doc.elm` is the TEA program,
so it cannot be the destination.)

**This feature is the reason to do it now.** The completion needs the FIRST half
and only the first half: which line opens a block, which opens a drawer, and
which names are verbatim. A second consumer of that half is what turns a
tidy-up into a seam — and this repo's own `/generalizer` doctrine lists "moving
code between files without changing its structure" among its ANTI-targets, so
without the second consumer this would be churn.

Measured, so the step is not hopeful:

- **Two cross-edges, both mis-filed helpers rather than coupling.** `apart` (a
  list splice) is defined at `:1299` in the insert section and called once from
  the splice at `:919`; `rowById` (a `Row` lookup) is defined at `:1401` under
  cursor and called from `:878`, `:1131`, `:1290`. `rowById` belongs beside
  `Row`, and `apart` beside the splice that uses it. Neither survives the move
  as a cross-module import.
- **The build's program list does not change.** `make elm` compiles
  `src/Listing.elm src/Doc.elm`; `Scan` is imported, never a program, so
  `TestSelfContained`'s "the committed Elm carries every program the build
  names" is untouched.
- **`assets/elm.js` gets a large mechanical diff** — it carries 315 `Scan$`
  symbols today, and roughly half become `Body$`. It is a committed build input,
  so `make elm` owes its step and the diff is reviewed as a rebuild rather than
  read.
- **`ScanTest.elm` needs no split.** It is one flat suite and can import both
  modules; partitioning it is a separate question nobody has asked.

Do this first, with the feature behind it. Alone it buys nothing.

## The rule

On commit, scan the typed text and collect every opener with no closer. Append
each one's closer at the end of the text, **innermost first** — a stack, so

```org
#+begin_quote
#+begin_src elisp
```

closes as `#+end_src` then `#+end_quote`.

Seven edges, each of which is a test:

| edge             | rule                                                                                                            |
|------------------|-----------------------------------------------------------------------------------------------------------------|
| nesting          | a stack; the last opened is the first closed                                                                    |
| verbatim         | inside `comment`, `example`, `export`, `src`, `verse` nothing counts — a `#+begin_quote` in a src block is text |
| case             | mirror the opener: `#+BEGIN_SRC` earns `#+END_SRC`                                                              |
| arguments        | `#+begin_src elisp` earns `#+end_src` — the NAME, never the args                                                |
| indentation      | the closer takes the opener's own indent                                                                        |
| `:END:`          | closes a drawer and opens nothing, so it is never an opener                                                     |
| balanced already | text that closes itself is returned unchanged                                                                   |

## The one real decision, and the corpus answers it

**A bare `:word:` line is plain TEXT in org today.** A drawer exists only once
its `:END:` does, so completing `:foo:` does not tidy a drawer — it *creates*
one. That is the only way this feature can surprise someone, so it was measured
rather than argued, over 6365 org files:

| line                               | count  |
|------------------------------------|--------|
| `:END:`                            | 21998  |
| `:PROPERTIES:`                     | 16881  |
| `:LOGBOOK:`                        | 5108   |
| `:TIMESTAMPS:`                     | 1      |
| `:results:`                        | 1      |
| `:Properties:` · `:End:` · `:end:` | 1 each |

**No bare `:word:` line is used as prose anywhere in the tree.** Three drawer
names carry it, all real drawers. So an allowlist guards against a case that
does not occur, and the plain rule — complete any drawer opener — is taken.

The case variants are the finding worth keeping: `:Properties:`, `:End:` and
`:end:` all appear, so org's case-insensitivity is used in the wild and mirroring
the opener's case is a rule rather than a nicety. `drawerEnds` (`Scan.elm:257`)
already uppercases before comparing, which is why they scan correctly today.

The same sweep found `#+begin_`/`#+end_` **balanced in every file**, which is
what makes the corpus oracle below meaningful: a real tree is closed, so any
change the completion makes to it is a false positive.

## Where point lands

After the commit the pane redraws. Point should land on the line INSIDE the
block, which is where typing continues; landing on the opener means the reader's
next keystroke is a movement. Worth doing, and separable — the feature is
useful before it.

## How it is proven

**The corpus is the oracle, and it is a strong one.** Real org files are
balanced, so running the completion over every body in `~/sync` — 12634
headlines, 6302 files — must change **nothing**. Any diff is a false positive,
and it finds the `:word:` class of error without anyone inventing a fixture.
`glance scan` already walks that tree.

Beside it:

- `elm-test` units, one per row of the edge table above;
- a browser case: type `#+begin_src` in the pane, commit, and read back two
  lines with point between them;
- an idempotence property: completing twice equals completing once.

## Cost

Step one moves 39 functions to `Body.elm` and re-homes two helpers. Then
`Scan.elm` gains one function and its port; `Doc.elm` calls it on the commit
path; `ScanTest.elm` gains the edge table; `assets/elm.js` is rebuilt, which is
a committed build input, so `make elm` owes its step. No Haskell changes, no new
vocabulary, no second copy of anything.


## What landed, 2026-08-14

Step one and the completion, both green. `Scan.closers` folds a stack over the
lines and `Body`'s splice appends what it returns to the row the reader just
moved, so the completion reaches exactly the text that was typed and no other.

Sixteen `elm-test` cases cover the edge table — nesting, verbatim suppression,
case, arguments, indent, drawers, stray `#+end_`, prose, and idempotence — and
one browser case reads the round trip back off the pane:
`"#+begin_src elisp\n#+end_src"`, the arguments the opener's alone.

Gates: 1976 tests, browser 11/11, elm-test 157, interop 13/13, check-glue clean.

**STILL OWED: the corpus oracle.** This document proposed running the completion
over every body in `~/sync` and asserting it changes nothing, and that was NOT
built — `closers` is Elm, and no harness runs it over the tree. What stands in
its place is the measurement taken while designing: blocks balanced in every
file, and no bare `:word:` line used as prose in 6365 files. That covers the
same risk by inspection rather than by execution, which is weaker, and the
difference is worth keeping visible.

**Where point lands is untouched.** After a commit the reader is on the block,
not inside it, exactly as before.
