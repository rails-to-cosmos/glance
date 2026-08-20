# Proposal — the wire contract covers field names, not kinds alone

**Status:** proposed · **Date:** 2026-08-20 · **Origin:** /generalizer — the
variant-cost sweep found the port family within budget except for one
unenforced half.

## Pattern

`TestSelfContained.hs:102-115` closes the `kind` word both ways — a kind the
shell sends and no program decodes, and a decoder branch nothing sends. The
*payload* is uncovered: `msgD` (`Doc.elm:1022-1087`) reads `D.field "by"`,
`"id"`, `"ids"`, `"text"`, `"at"`, `"props"`, `"plan"`, and a glue site
spelling one wrong fails `D.decodeValue` into `Result.withDefault Ignore`
(`Doc.elm:2019`) — a silent no-op with no console line and no failing test.
`glue.d.ts:64` types every send as `{ kind: string } & Record<string, any>`,
so tsc cannot see it either.

## Proposed change

Extend the same textual harness one field deep:

- `kindsIn` grows a fields half: for each `"x" ->` branch, collect the
  `D.field "…"` names its decoder body reads (they sit in the same `let`
  block, the harness's own prefix-grep style).
- `sendsIn` likewise collects the keys of each `dsend({kind: "x", …})`
  object literal.
- The test asserts, per kind: every field the decoder requires appears in
  every send of that kind. (Optional fields via `D.maybe` are exempt by
  matching only `D.field`.)

The harness stays textual on purpose — the same reasoning as the keymap
oracle (`TestServe.hs:9110`): an oracle generated from the code would agree
with anything.

## LOC estimate

+35 in TestSelfContained / −0; every future port message gets its payload
checked for free.

## Risk

None to runtime; the risk is harness brittleness (a send built not as one
literal escapes the grep — it already escapes `sendsIn` today, so no
regression).

## Existing precedent

`TestSelfContained.hs:102-115` — the kind-word half of the same contract.
