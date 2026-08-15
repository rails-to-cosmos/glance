# Proposal — the keymap/glue join, checked

**Status:** done — the `VERBED` case landed as `TestServe.hs` "every command
that names rows spells its own log phrase"; the `HANDLERS` case landed
2026-08-15, proven by typing one `kbHandler` wrong and watching it go red.
The optional second step (`kbWrites`) is declined: it moves the served keymap
blob, which is a wire change wearing a test's clothes ·
**Date:** 2026-08-04

`Glance.Web.Keymap` and `Glance.Web.Page.Glue` are two hand-maintained lists of
~50 identifiers in two languages inside ONE component, joined by string equality
at runtime and by nothing at build time.  A row whose `kbHandler` names a
function the glue does not carry is bound, documented, drawn on the key line,
echoed by the widget — and dead.

## The join

`Glance.Web.Keymap.keyBindings` (`src-web/Glance/Web/Keymap.hs:64-202`, 47 rows)
carries `kbHandler :: !(Maybe Text)`, serialized into the blob at
`Keymap.hs:329-334`.  The page resolves it at `Glue.hs:4358-4361`:

```javascript
const handler = b.handler && HANDLERS[b.handler];
if (handler) handler(b);
else append("cmd", "info", …arrives with daemon commands (M4));
```

`Nothing` is a deliberately staged row and prints that M4 line.  A TYPO prints
the same line.  The two are indistinguishable at runtime and neither is
distinguishable at build time — the only mention of `HANDLERS` in the suite is
the substring `"HANDLERS[b.handler]"` at `test/TestServe.hs:5915`, which asserts
that the lookup is spelled, never that it resolves.

The same shape sits one list over.  `POST /command`'s ten names
(`Glance.Web.Commands.commands`, `Commands.hs:131-174`) each owe a `VERBED`
phrase at `Glue.hs:2371-2382`, read at `:2384`:

```javascript
const verbed = (name, args, verb) => (VERBED[name] || stated)(args, verb);
```

A missing entry falls back to `set-state`'s phrasing, so a new command logs
`→ undefined` or `state cleared` over rows it moved.  `Glue.hs:2367-2368` states
the obligation in prose ("a name added to the route is a line here") with no test
behind it.  `grep -rn "commandNames\|Glance.Web.Commands" test/` is empty.

## Why this one first

Every other list in the family is already quantified.  `test/TestServe.hs`
asserts that no sequence is bound twice and none opens a longer one (`:9727`),
that every `keyHints` command resolves to a bound row (`:9498`), that every
`onceCommands` entry names a real command (`:9823`), that every `reservedChords`
entry is unbound alone (`:9812`), and that every command name is typeable as one
word (`:9718`).  The ONE link left unchecked is the one that decides whether a
key does anything.

The command table went from four members to ten inside the window `CLAUDE.md`
describes, so member eleven is near-certain, and it costs two silent
registrations out of thirteen.

## Proposed change

One tasty case, built on machinery that already exists: `glueOf`
(`test/TestServe.hs:9681`) extracts the inline script and `keymapOf` (`:9673`)
parses the blob.  Extract `HANDLERS`' and `VERBED`'s keys with the same `between`
helper and assert three set inclusions.

```haskell
-- Three joins the compiler cannot see: the keymap names shell functions, the
-- command table names log phrases, and both are matched by string equality in
-- a language the solver never reads.  An unresolved handler prints the same
-- line a deliberately staged row prints, so nothing distinguishes a typo from
-- an intention without asking the two lists whether they agree.
, testCase "a binding names a handler the shell carries" $ do
    rows     <- keymapOf shell
    handlers <- objectKeys "HANDLERS" (glueOf shell)
    assertBool "the sweep found handlers" (length handlers >= 20)
    assertEqual "bound to a handler the glue does not define" []
      [ h | (_k, _s, _c, Just h, _scope, _help) <- rows, h `notElem` handlers ]

, testCase "a command names a phrase the log can speak" $ do
    verbed <- objectKeys "VERBED" (glueOf shell)
    assertEqual "commands with no phrase and no blessed fallback" []
      [ n | n <- commandNames, n `notElem` verbed, n `notElem` phraseless ]
```

`phraseless` is the short list of commands that legitimately take `stated`'s
default (`set-state`, `capture`), spelled once beside the case so a new one is a
decision rather than an omission.  `objectKeys` reads `NAME = {` to its matching
brace and collects the identifiers at key position, including the shorthand group
at `Glue.hs:4253` (`applyDefault`, `relations`, `focusFilter`, `toggleRaw`,
`openSettings`).

The anti-vacuity line matters: `TestSelfContained` already establishes the house
rule that a sweep asserts what it swept before it asserts what it found.

## A second step, optional

Extend `KeyBinding` with `kbWrites :: !(Maybe Text)` naming the `/command` name a
row fires, and assert `kbWrites ∈ commandNames`.  That makes the keymap say which
rows write, which is the fact `onceCommands` currently encodes by hand
(`Keymap.hs:255-271`) and the reason `archive-flag` needs it most.

## LOC

Added ~14 (one helper, two cases).  Removed 0.  Saved per future binding and per
future command: the assertion is quantified over `keyBindings` and
`commandNames`, so member N+1 is covered the day it lands, at zero marginal cost.

## Risk

Test-only.  No production module changes, no wire field moves, no page byte
moves.  The one way it can fail is `objectKeys` mis-parsing the glue's object
literal, which the `length handlers >= 20` guard turns into a loud failure rather
than a vacuous pass.

## Existing precedent

`test/TestServe.hs:9498-9501` is this exact assertion for `keyHints`:
`[ c | (cs,_) <- hints, c <- cs, c `notElem` offered ]`.  The proposal is that
list comprehension pointed at the join that was left out.
