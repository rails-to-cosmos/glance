# Proposal — the value palette's ask kinds as a closed set

**Status:** proposed (re-validated) · **Date:** 2026-08-04 · **Source:** generalizer sweep over
the capture-v2 territory

**Re-validated 2026-08-10:** HOLDS, and is STRONGER than when written.  The
flag space GREW a third member: `drawChoices' now reads
`prompting.table && !prompting.narrow && !prompting.text', so the undefined
combinations went from one pair to a triple.  `askOn' is gone (three doors,
not four).  Deferred as MEDIUM risk: the palette is load-bearing for `t',
`P', `:', `+' and the template box's `%'.
## Pattern

The value palette has three ask kinds — letter mode, completing-field mode,
text mode — discriminated by two booleans on one mutable `prompting` object
(`narrow`, `text`), read at three dispatch points (the key listener
`Glue.hs:4581/4594`, `drawChoices` `:2956-2961`, `entry`'s marks
`:2995-2996`), entered through four doors (`ask`, `askFrom`, `askText`, and
capture v2's `askOn`), each hand-deciding the `raising` guard differently
(`ask` leaves it set, `askFrom` clears inline `:2915`, `askOn` clears after
`:2674`).

Nothing makes the flag space coherent: `text` + `narrow` together is undefined
and nothing refuses it. Two near-miss kinds already dodged the family by
reusing text mode — the date ask (`askOn` sending `date`) and the two-field
ask (the edit overlay's shape) — which is evidence both that readers keep
needing new kinds and that the family resists growth.

Beside it, eight one-off foot strings of the shape
`"RET <verb>s it · … · ESC leaves"`, one copied per ask, no oracle.

## Measured cost of kind #4

(A date ask with validation, a multi-field ask.) A new flag or combination, a
branch at each of the three dispatch points, usually a fifth door, and a
`raising` decision — ≈4-6 sites with no rule for combinations.

## Proposed change

One `kind` field replaces the boolean pair:

```js
// prompting.kind: "letters" | "field" | "text"
```

- The three dispatch points switch on `kind` — illegal combinations become
  unrepresentable, `drawChoices` and the marks read one word.
- The four doors become one `raise(kind, config)` with per-kind config
  (choices, foot, seed); `ask`/`askFrom`/`askText`/`askOn` stay as one-line
  callers or dissolve.
- The `raising` decision moves into `raise` (one rule: set on synchronous
  raise, cleared by the chain door) — note this interacts with the held
  simplify finding that derives `raising` from event identity; either
  resolution slots into `raise` unchanged, which is the point of one door.
- Foot strings become per-kind defaults with an override, one spelling of the
  `ESC leaves` tail.

## Files

`src-web/Glance/Web/Page/Glue.hs` (single-module), `test/TestServe.hs`
(palette groups re-pin through the kinds).

## LOC estimate

+~20 / −~25 immediately / **−4 to −6 per future kind** (one dispatch arm + one
config object instead of flag surgery at three points).

## Risk

No wire change, no server change. The palette's behavior tests are extensive
(state palette, capture chain, tags `+` field) and all behavioral — they
should pass unmoved; a literal-pin sweep on `prompting.narrow`/`prompting.text`
in TestServe's glue needles must be re-aimed at `kind`.

## When

With the held ask-chain simplify items (CPS→async, `raising` derivation) —
the three touch one mechanism and want one review; separately they each
re-plumb `prompting` twice.

## Existing precedent

`SURFACES` (the modal registry with per-entry config), `grain` on document
rows (`element`/`composite`/`leaf` — a kind word, not flag pairs), the
keymap's `kbScope` as a closed vocabulary.
