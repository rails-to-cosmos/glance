# Proposal — a command's KIND, rather than a Maybe and two string tests

**Status:** done — DONE 2026-08-10 · **Date:** 2026-08-10 · **Found by:** /generalizer over
`e1ba099..6412f4a` (the `delete` command)

## Pattern

`CommandSpec.csEdits :: Maybe RowEdits` (`Commands.hs:145`) carried one bit —
"does this command edit its rows" — and the table had one member on the
`Nothing` side, so the bit and the member were the same fact. `delete` made it
two, and the type stopped saying which.

What the code now does instead, in two places that must agree and nothing
enforces:

```haskell
-- Commands.hs:328
Right cmd -> case csEdits (cmdSpec cmd) of
  Nothing | cmdName cmd == "delete" -> deleteRows opts hub st cmd
          | otherwise               -> captureInto opts hub st cmd

-- Commands.hs:653, the id wall
| name /= "capture", null ids -> Left "a command names rows: …"
```

Two string comparisons standing in for a fact the spec should carry, in
different modules of the same file, reached by different questions ("is it
delete?" / "is it not capture?"). A third edit-less command is correct only if
whoever adds it remembers both — and `otherwise` will silently run it as a
CAPTURE. `cmdName` was added to `Command` for the first of these, so the type
already grew a field to support the string test.

## Files

- `src-web/Glance/Web/Commands.hs:141-146` — `CommandSpec`
- `src-web/Glance/Web/Commands.hs:328-330` — the dispatch
- `src-web/Glance/Web/Commands.hs:653` — the id wall
- `src-web/Glance/Web/Commands.hs:160-…` — `commands`, eleven entries

## Proposed change

Replace the `Maybe` with the sum it has become:

```haskell
-- | WHAT A COMMAND DOES TO THE ROWS IT NAMES.  A closed set, so the dispatch
-- is total and a fourth kind is named by the compiler at every site rather
-- than falling through to whichever arm `otherwise' reaches.
data CommandKind
  = Splices RowEdits   -- ^ edits each named row in place.
  | Makes              -- ^ makes a row: `capture', which owes no ids.
  | Moves              -- ^ moves a file out of the tree: `delete'.

data CommandSpec = CommandSpec
  { csArgs  :: [Text] -> Args -> Maybe Text
  , csDated :: Bool
  , csKind  :: CommandKind
  }
```

Then the dispatch is total and mentions no name:

```haskell
Right cmd -> case csKind (cmdSpec cmd) of
  Moves       -> deleteRows opts hub st cmd
  Makes       -> captureInto opts hub st cmd
  Splices row -> do asked <- resolveAsked cmd
                    either (pure . jsonError status400) id
                           (asked >>= \at -> overRows opts hub st at row cmd)
```

and the id wall asks the kind rather than the name:

```haskell
-- `Makes' is the one kind that owes no ids: it makes a row rather than
-- naming one.
| Makes <- csKind spec, not (null ids) -> Left "capture names no rows"
| Makes /= csKind spec, null ids       -> Left "a command names rows: …"
```

(`CommandKind` needs no `Eq` if the wall is written as a `case` — spelled here
for brevity.)

`cmdName` stays: it is worth carrying for the echo and for error text, and
nothing then dispatches on it.

## LOC estimate

- Added: ~8 (the type and its haddock).
- Removed immediately: the two string tests, and the `otherwise` arm that makes
  an unknown edit-less command a capture.
- Saved per future variant: nothing measurable in lines — the win is that a
  fourth kind is NAMED BY THE COMPILER instead of silently mis-dispatched.
  MEASURED on implementing, by adding a fourth constructor: **six sites**, not
  the three estimated. They are `-Wincomplete-patterns` WARNINGS rather than
  errors, there being no `-Werror` in any stanza — which is exactly what
  `docs/proposals/done/2026-08-04-generalize-closed-sums.md` asks for, and this finding now
  depends on it for the guarantee to be a wall rather than a log line.

## Risk

None outside the module. `CommandSpec` is not exported beyond `Glance.Web`;
the wire shape, the table's names and every refusal string are untouched. The
suite's command cases pin behaviour rather than the constructor, so a green run
is the check.

## Existing precedent

The repo already does this and says why: `Data.Org.Types:184-187` keeps
`Element`'s sum closed with no catch-all so that a fifth constructor is named
by the compiler at three sites, and `Glance.Web.Store.frameJSON`
(`Store.hs:448-452`) does the same for `Frame`. `docs/proposals/done/2026-08-04-generalize-closed-sums.md`
argues for closing the two sums that still carry catch-alls; this is the same
argument one step earlier — a sum that has not been written down yet.
