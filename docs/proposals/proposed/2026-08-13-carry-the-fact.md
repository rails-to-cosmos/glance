# Proposal — carry the fact, rather than re-deriving it downstream

**Status:** proposed — 2026-08-13 ·
**Date:** 2026-08-13 · **Origin:** `/generalizer`, the abstraction-shape and
cross-cut angles, over the region-scanner and notification-fold work of
2026-08-12/13.

## The class

Ten rounds of review over two subsystems produced findings that look unrelated
and are one shape: **a fact is known on the branch that decides, thrown away,
and re-derived by something downstream that has less to go on.** Every instance
below was found that way, and two of them are live defects rather than
untidiness.

The repo has ruled on this class twice already, in Haskell, and both rulings
are quoted in CLAUDE.md:

- `RowWrite` — *"A ROW'S ANSWER CARRIES BOTH … Two table fields would have to
  agree about org's two-part condition, and asking them apart ran `repeatOn` —
  and so `keywordSources` — twice a row for one write."*
- `Asked` / `ConfigParts` — *"a RECORD rather than three positional `Maybe
  Text` (all three the same type, so a caller swapping two would compile)"*.

`Join.word` landed this session for exactly this reason and is the in-repo
precedent one type away from every item below.

## 1. `Stop.name` is a string where `RegionKind` is a sum — and it collides

`Stop`/`Row` carry `name : Maybe String`, holding either a structural tag
(`"list"`, `"table"`) or the author's own block name. `sibling` then asks
`top.name == Just "list" && r.grain == Leaf`.

So a tree writing

```org
#+begin_list
- a
- b
#+end_list
```

produces a composite NAMED `"list"` whose inner runs are `Leaf` stops. `+`
inside it takes the list-item branch: the marker degrades to an indent, `alone`
is false, and the line lands **inside the block** instead of past it, with the
echo saying "after the list". `#+begin_table` is the same one string over.
Neither name is verbatim, so both are greater regions and both are reachable.

**The right shape is nine lines above the wrong one.** `Region` carries
`kind : RegionKind` and reads the block's authored name off the LINE
(`blockName (at reg.from lines)`) rather than off a stored string. `sibling`
should case on the kind.

`Doc.elm` compounds it, using `name` as prose with `"item"` as a fallback in
one branch and `kindWord up.kind` in the other — one field doing structure and
display at once.

## 2. The echo names the wrong key

`insertHere` hardcodes the `INSERT` binding (`"+"`), so `M-RET` and `S-RET`
outside a box both echo `+ → org-insert-element`, and `S-RET` **inside** a box
emits two lines — its own, then the chained insert's. `NEXT` exists precisely
to carry the key that fired and is not threaded past the commit.

CLAUDE.md: *"the ECHO speaks them verbatim: `SEQ → command` … since the
rebinding config to come will address a function by exactly this string."*

**Fix.** `insertHere` takes the binding that fired.

## 3. `joinAt` runs twice per keypress

`Doc.update` asks `drafted` and `joinWord` for a `Draft`, then `insertion` and
`joinLine` for an `Insert`. Each of the four calls `joinAt`, so the recursive
region walk runs twice for one press — and the `case ( a, b ) of` asks the
compiler to handle a both-or-neither state that cannot occur.

**Fix.** One `Join`, destructured. `(id, caret)` becomes one record, which also
un-swaps `Insert String (Maybe Int) String` — two `String`s around a
`Maybe Int`, positional in a `map3`.

## 4. `caretIn` re-parses the marker to recover the kind

`inside` has `reg.kind` in hand, calls `markerFor`, and drops it. `stateJSON`
then calls `caretIn` on the drawn row's TEXT, which re-discovers table-ness by
parsing the marker string back apart.

This is `Join.word`'s own argument, unapplied one field over: *"The word rides
here because every branch below already knows it."*

**Fix.** `caret : Int` beside `marker` on `Join`, minted where the kind is
known.

## 5. `at` changes coordinate frame between where it is read and where it is used

`commitDocEdit(b, at)` takes a line index into the BOX's text. On success the
chained `more()` hands the same number to `insertHere`, which reads
`docRowAt()` — the row at point AFTER the rescan, which is the row the commit
just made. It survives only because `caretLine` clamps.

`insertion`'s own docstring states the rule for the other half of the pair:
*"It takes the DRAFT'S OWN CARET … reading a different line here would ride the
reader's second line under a bullet it never wore."* The chained insert is the
one place it is not applied.

## 6. The shell still spells one piece of org grammar

`20-sheet.js`'s `CHECKBOX` regex carries org's bullet grammar and its four box
states — the facts `Scan.listOpener` and `Scan.boxAfter` own. `insertWord` was
deleted this session for exactly this; `CHECKBOX` is the remaining instance in
the same file.

Also: the shell finds the draft with a literal `"D"` where `Scan.draftId` is
exported and never crosses the wire.

## 7. Two per-repo duplications of one fold

**org-glance.** `--latest-records` and `--read-external` are the same fold —
first-seen position, last-seen value — written twice. The evidence that this
drifts is in the same session's diff: `--read-external` carried a quadratic
`assoc` while `--latest-records` already had the hash table, so the fix landed
in the copy that was wrong and could have landed in neither.

**org-glance.** The generation family is spelled twice, segment and
notification: stem, `%010d` name, regexp, number-out, listing, and `1+ max` —
the last of which exists as a named function on one side and inline on the
other, so nothing keeps them in step.

## What this is worth

Items 1 and 2 are defects. Items 3–6 are one class with an in-repo precedent
and a named fix each. Item 7 is where the same class already cost a
performance bug this session.

Not proposed, deliberately: collapsing the two `EXTERNAL.jsonl` line spellers.
That duplication is designed — each side pins the format independently and by
hand, which is what makes a renamed field fail somewhere.

## And one doctrine that arrived without its instrument

org-glance borrowed glance's *deletion is a move* rule for generation
retirement and did not borrow the reporting that ships beside it. glance's
`scan` says `unmatched N unindexed blobs, N records without blobs`; the peer's
`spent/` has no reader at all, and `clear-spent-external` deletes the directory
with no check — so the recoverability the design bought is spendable by the one
command written to spend it.

The mirror is also true and is the older gap: **glance's own trash has no
pruner.** `trashDirIn` is reached by a re-export and a self-containment test,
and gzipped blob directories have accumulated there since the feature shipped.
