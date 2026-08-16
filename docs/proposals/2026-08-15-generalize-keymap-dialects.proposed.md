# Proposal — three movement dialects, spelled as fourteen rows

**Status:** proposed · **Date:** 2026-08-15 · **Origin:** `/generalizer`,
cross-cut angle.

## The pattern

`src-web/Glance/Web/Keymap.hs:33-55` binds five commands through fourteen rows,
because the shell offers three dialects for the same movement — org-glance's own
letters, vi's, and the arrow keys:

| command | rows |
| --- | --- |
| `next-row` | `n`, `j`, `<down>` (`:33`, `:35`, `:37`) |
| `previous-row` | `p`, `k`, `<up>` (`:34`, `:36`, `:38`) |
| `next-column` | `f`, `l`, `<right>` (`:39`, `:43`, `:47`) |
| `previous-column` | `b`, `h`, `<left>` (`:41`, `:45`, `:49`) |
| `last-row` | `>`, `G` (`:53`, `:55`) |

Each row restates the command name, the handler, the scope and — for the four
column rows — a `helps` continuation, so the six column bindings occupy twelve
source lines carrying two distinct facts. `nextColumnHelp` and
`previousColumnHelp` exist only because the help text would otherwise be typed
three times each.

**The aliasing is an invariant, and it is held by hand.** That `l`/`h`/arrows
always mean what `f`/`b` mean, and `j`/`k`/arrows what `n`/`p` mean, is a design
rule; nothing in the type stops a future edit rebinding `l` while `f` keeps its
meaning, and no test would notice.

## Proposed change

A row gains its alternative spellings, and the JSON expands them. `kbKeys` is a
key SEQUENCE and the page's dispatch matches on `seq`, so the wire must keep one
row per spelling — this is a source-level fold that expands at emit time, and
the blob the page receives is byte-identical.

```haskell
data KeyBinding = KeyBinding
  { kbKeys    :: ![Text]
  , kbAlso    :: ![[Text]]      -- ^ other spellings of the SAME command; see 'expand'.
  , kbCommand :: !Text
  , kbHandler :: !(Maybe Text)
  , kbScope   :: !Text
  , kbHelp    :: !(Maybe Text)
  }

-- | Alternative spellings, in the order the key line would show them.
also :: KeyBinding -> [[Text]] -> KeyBinding
also b ks = b { kbAlso = ks }

-- | One row per spelling, the declared keys first — which is what 'keyHints'
-- reads when it shows a command's FIRST row.
expand :: KeyBinding -> [KeyBinding]
expand b = b : [ b { kbKeys = ks, kbAlso = [] } | ks <- kbAlso b ]
```

The fourteen rows become five:

```haskell
  [ bind ["n"] "next-row"        (Just "nextRow")        "table" `also` [["j"], ["<down>"]]
  , bind ["p"] "previous-row"    (Just "previousRow")    "table" `also` [["k"], ["<up>"]]
  , bind ["f"] "next-column"     (Just "nextColumn")     "table" `also` [["l"], ["<right>"]]
      `helps` nextColumnHelp
  , bind ["b"] "previous-column" (Just "previousColumn") "table" `also` [["h"], ["<left>"]]
      `helps` previousColumnHelp
  , bind [">"] "last-row"        (Just "lastRow")        "table" `also` [["G"]]
      `helps` lastRowHelp
  ]
```

with `keyBindingsJSON` mapping `expand` over the list before it builds rows.
`nextColumnHelp` and `previousColumnHelp` can then be inlined at their single
use, or kept — either way they stop being a workaround for triplication.

## LOC

Added ~8 (the field, `also`, `expand`). Removed ~20 now (nine surplus rows and
their `helps` continuations, and two help bindings that exist only to be shared).
**Saved per future dialect: one bracket on an existing row instead of a full row
per command — a fourth dialect over the four movement commands is four tokens
rather than four rows.**

## Risk

`src-web/Glance/Web/Keymap.hs` only. The emitted JSON must come out unchanged,
including row ORDER: `keyHints` reads a command's first row for the key line
(`:32`, `:158-163`), and `expand` puts the declared keys first to preserve it.
Pin it with a test asserting `keyBindingsJSON` before and after are equal, or
assert the expanded `[(seq, command)]` list against the current fourteen. No
wire field moves, no org bytes, and the page's dispatch is untouched.

## Existing precedent

The file already does registry-with-derived-rows: `keyHints` (`:158`) is a
second table derived from the same commands, and `helps` is already a combinator
modifying a row after `bind`. `also` is `helps`'s sibling.

## What would say this was wrong

A dialect turns out to need its own scope or its own handler — an arrow key that
should also work in `modal` where the letter should not. Then the spellings are
not aliases and want their own rows after all. Check `kbScope` across the
fourteen before folding: today all fourteen are `table`.
