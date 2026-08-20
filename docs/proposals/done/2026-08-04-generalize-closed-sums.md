# Proposal — make the closed sums actually closed

**Status:** done — DONE 2026-08-10 · **Date:** 2026-08-04

Four modules spell every constructor of a sum with no catch-all, and say in
their own comments that this is what makes the obligation enforceable.  Two more
spell a catch-all over the same kind of sum, and the compiler goes quiet exactly
where the guarantee was wanted.  Meanwhile the enforcement the honest four rely
on is a warning in a build log: there is no `-Werror` in any stanza.

## What is already right

`Data.Org.Types:184-187` states the design intent and honours it —
`instance Display Element` (`:169-172`), `instance TextShow Element` (`:175-178`)
and `stripSpans` (`:188-192`) each spell all four constructors with no wildcard,
so a fifth `Element` is named by the compiler at three sites.  CLAUDE.md asks
whether the `stripSpans` obligation is enforced or prose: it is enforced, by
`-Wincomplete-patterns` under `-Wall`.

`Glance.Web.Store.frameJSON` (`Store.hs:448-452`) does the same for `Frame`, and
`Glance.Web.Filter.fieldCells` (`Filter.hs:381-385`) for `Field`.

## What is wrong

**Two catch-alls make `Field` a sum that only half enforces closure.**

`Glance.Web.Filter.keyTest` ends at `Filter.hs:473`:

```haskell
keyTest _env key field value = …
```

`field` is bound to a variable, so a fifth `Field` constructor falls into the
column-cell arm, reads `fieldCells`' empty list and matches nothing — with no
warning.  `valueFor` (`:409`) ends the same way, `valueFor _ = T.toLower . tmValue`,
so a new key that must not be case-folded is silently folded.

The cost is asymmetric and worth stating plainly: `fieldCells` is exhaustive, so
a contributor adding `Field` #5 is told about ONE of the four sites it must
visit, and reasonably concludes the other three are derived.

**The enforcement everything above rests on is non-fatal.**  `-Wall` (implying
`-Wincomplete-patterns`) is on for all six stanzas —
`glance.cabal:67, 99, 137, 170, 202, 247` — and `-Werror` appears in none of
them.  So `Types.hs:184-187`'s "the compiler now asks for the arm" means the
compiler prints a line a contributor scrolls past on the way to a green suite.

**Two more sums take their own values opaquely.**  `Store.installed`
(`Store.hs:339-340`) and `Store.guarded` (`:353`) both take `[Frame]` and never
match on it; `guarded` special-cases `ViewChanged` by CONSTRUCTING it.  A fifth
frame that also has to replace rather than append gets appended, quietly.

## Proposed change

Three edits, in rising order of blast radius.

**1. Delete the two catch-alls.**  Zero behaviour change; the current catch-all
body becomes the `Col`/`Planned` arm, which is what it already means.

```haskell
-- One equation per constructor and no wildcard, so a fifth key is named here
-- by the compiler rather than folded into the column arm and found to match
-- nothing.
valueFor :: Field -> Term -> Text
valueFor Ref       = tmValue
valueFor (Col _)   = T.toLower . tmValue
valueFor Planned   = T.toLower . tmValue
valueFor Order     = T.toLower . tmValue
```

and the same shape for `keyTest`'s final equation.

**2. Add `-Werror=incomplete-patterns` to the six `ghc-options` lines.**  It is
narrower than `-Werror`, which would break the build on `-Wredundant-constraints`
churn, and it is exactly the guarantee `Types.hs:184-187`, `Store.hs:448` and
`Filter.hs:381` are each already relying on.  Nothing in the tree currently
warns, so the change is inert until someone adds a constructor.

**3. Split `Frame` so the compiler can tell a message from a close.**  The
`Maybe` in `frameJSON`'s return type encodes that distinction by convention
today; making it a type moves `guarded`'s replace-rather-append rule from a
constructor comparison into a `case`.

```haskell
-- | What a live client receives.  A ROW OP travels as a message; a CLOSE
-- travels as a reason, and 'guarded' REPLACES a step's ops with one, since rows
-- built against a view that has already moved are rows a client draws wrong.
data Frame = Op !RowOp | Close !CloseReason

data RowOp = SetRows ![Value] | UpsertRow !Value | DeleteRow !Text

-- | The whole vocabulary of a server-initiated close.
data CloseReason = ViewChanged | Resync
  deriving (Show, Eq, Enum, Bounded)

closeReason :: CloseReason -> Text
```

`Bounded` gives the wire vocabulary a generated list where two hand-typed strings
sit now (`Routes.hs:1117`, `:1120`), and `guarded` gains an arm the compiler
names for close reason #3.

## LOC

Added ~16 (three `valueFor` equations, three `keyTest` equations, the `Frame`
split, `closeReason`).  Removed ~4 (two catch-alls, the `ViewChanged` comparison).
Six tokens in the cabal file.  Saved per future filter key: 2 silent sites become
2 compiler-named ones.  Per future frame: 2 silent sites become 1 named one.

## Risk

Steps 1 and 2 carry none — no behaviour changes, no wire field moves, and step 2
is inert on a tree that does not warn.  Step 3 touches `Glance.Web.Store`'s
exported `Frame`, which `Glance.Web.Routes` imports (`Routes.hs:93`) and
`test/TestStore.hs:731-734` constructs; the JSON `frameJSON` emits is unchanged,
so `table-view/SCHEMA.md`'s streaming ops and the client's `op` switch stay put.
Step 3 can be declined without losing steps 1 and 2.

## Existing precedent

`Data.Org.Types:184-192` is the argument, written by this codebase about itself:
"Every constructor is spelled out and there is NO catch-all, which is what makes
the obligation enforceable … Under `-Wall` the compiler now asks for the arm."
The proposal is to make that sentence true everywhere it is claimed, and to make
"asks" mean the build stops.

## What it turned out to be, on implementing

All three edits landed and the suite stayed green throughout, which is what a
zero-behaviour-change refactor owes.

`-Werror=incomplete-patterns` went into all SEVEN `ghc-options` lines, not six —
the count in this proposal predates a stanza. Nothing in the tree warned, so it
was inert on landing, and it is inert until someone adds a constructor. MEASURED
after: a fifth `Field` is now a build ERROR at three sites (`fieldCells`,
`valueFor`, `keyTest`) where it used to be a warning at one; a third
`CloseReason` is an error at `closeReason`.

`keyTest`'s catch-all did not become two equations but one named helper,
`cellsTest`, that `Col` and `Planned` both call: the two arms are the same body,
and spelling it twice to satisfy the compiler would have been the duplication
this proposal exists to remove.

`Resync` was NOT a `Frame` before — it was the mailbox-overflow branch reading
`Nothing` off the mailbox, with its string typed at the call site. Folding it
into `CloseReason` is what makes "the whole vocabulary of a server-initiated
close" a type rather than a sentence in CLAUDE.md.
