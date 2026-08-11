# Proposal — a capture shape resolved once

**Status:** proposed (re-validated, premature) · **Date:** 2026-08-04 · **Source:** generalizer sweep over
the capture-v2 territory

**Re-validated 2026-08-10:** the five sites are all still there
(`Commands.hs:465`'s `maybe captureInbox captureBlob`, the shell's `!tag`,
`Routes.hs:813`).  Held back anyway: shape #3 is hypothetical — this proposal
says so itself — and generalizing a two-member family over a variant nobody
has asked for is the skill's own anti-target.  Revisit when a third target
shape is actually wanted.
## Pattern

Capture has two target shapes — untagged inbox, tagged blob — and the
discriminator (tag presence) is re-tested at five independent places:

- `wantsText`'s tag guard (`Commands.hs:206`)
- `captureInto`'s `maybe captureInbox captureBlob` (`:357`)
- the shell's `captureUnder` `if (!tag)` (`Glue.hs:2649`)
- `captureRow`'s tag ternaries (args + echo)
- `GET /capture`'s `queryText "tag"` branch (`Routes.hs:820`)

The response contract is already unified (`{ok, file, digest, id}` both ways),
which is the one place a third shape rides free.

## Measured cost of shape #3

(A named-file target, a date-tree, a project shape — none proposed yet.) A new
`Args` field + parse line; a precedence rule `maybe` cannot express (tag AND
the new discriminator both present is a refusal nobody has written); a
restructured `captureInto`; a `captureX` (~40-90 lines); a `/capture` probe
branch; a third arm in the shell chain plus three-way ternaries; harness stub;
a TestServe group; docs. **≈12 mechanical sites across 4 source + 2 test
files** before any unique logic.

## Proposed change

Resolve the shape ONCE per request, server-side, and let everything downstream
switch on the resolution rather than on raw args:

```haskell
data CaptureShape
  = InboxCapture                       -- the bare path, unchanged
  | BlobCapture !Text                  -- the tag
  -- a future member carries its own data

resolveCapture :: Args -> Either Text CaptureShape
```

`wantsText` keeps the charset wall; `resolveCapture` owns presence/precedence
(a both-discriminators request is its refusal, written once); `captureInto`
cases the sum. The shell's chain gets the mirror: `/capture`'s answer grows a
`shape` field the chain switches on once, collapsing the scattered `if (tag)`s
into one dispatch — additive on the wire, absent field = the two-shape reading.

## Files

`src-web/Glance/Web/Commands.hs`, `src-web/Glance/Web/Routes.hs`,
`src-web/Glance/Web/Page/Glue.hs`, `test/TestServe.hs`,
`test/fixtures/shell-harness.js`.

## LOC estimate

+~25 / −~15 immediately / **−8 to −10 per future shape** (the five binaries
become one case arm + one chain arm).

## Risk

Wire: one additive field. Behavior: none intended; the both-present refusal is
new surface (today unreachable — only `tag` exists). Test baselines: the
capture groups re-pin through the sum's arms.

## When

**Not yet.** Two members, no third proposed (docs/proposal-capture.done.md is
delivered and names none). This is a cost recorded so the third member's
author finds the design ready rather than re-deriving it; pre-paying now buys
nothing and adds a sum with two constructors.

## Existing precedent

`parseCommand` resolving the name before anything else ("a `Command` cannot be
built without the entry it resolved to"); `TemplatePart` as the scan's closed
sum.
