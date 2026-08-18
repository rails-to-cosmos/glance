# Proposal — the port vocabulary is joined, like every other list of two places

**Status:** proposed · **Date:** 2026-08-18 · **Origin:** `/generalizer` audit,
weighed against the code it names.

## The finding, in one line

Port NAMES are already joined against their Elm declarations
(`TestSelfContained.hs:142-153`); the vocabulary carried OVER those ports is the
one cross-language string set left unjoined, and both decoders end
`_ -> D.succeed Ignore`.

## The pattern

The shell talks to its two Elm programs through `{kind: "…"}` objects:

- `Doc.msgD` decodes 15 kinds (`Doc.elm:564-610`);
- `Listing.msgD` decodes 7 (`Listing.elm:234-255`);
- the glue writes 23 `kind: "…"` literals, all in `20-sheet.js`;
- 17 distinct names each side, and today the two sets agree exactly.

Nothing joins them. `glue.d.ts:24` and `:55` type the send as
`{ kind: string } & Record<string, any>`, so `make check-glue` cannot see it
either, and an unrecognised kind is dropped without a word.

The node harness does mount both programs for real, so a mis-spelled kind on an
existing key fails some `TestServe` case — but that catch is **behavioural and
indirect**: it fires only if a case happens to press that key, and it is vacuous
when node is off `PATH`. It cannot see the dead direction at all — a decoder
branch nobody sends, or a `Msg` whose kind string was renamed on one side.

## The change

One case in `test/TestSelfContained.hs`, in the idiom that file already uses four
times over:

```haskell
, testCase "every port kind the shell sends is one an Elm program decodes" $ do
    doc  <- kindsOf "frontend/elm/src/Doc.elm"
    list <- kindsOf "frontend/elm/src/Listing.elm"
    sent <- concat <$> mapM sendsIn gluePartFiles
    -- THE SWEEP GUARD FIRST: a regex that stops matching must be loud, not green.
    assertBool ("too few kinds swept: " <> show (length doc, length list, length sent))
               (length doc >= 10 && length sent >= 15)
    assertEqual "a kind the shell sends and no program decodes" []
                (nub sent \\ (doc <> list))
    assertEqual "a decoder branch nothing sends" []
                ((doc <> list) \\ nub sent)
```

The two sets are a **union** rather than per-program on purpose: `flagPort`
(`20-sheet.js:14-21`) is one constructor serving whichever program holds its rows,
so `flag`, `unflag`, `clearFlags` and `step` are legitimately sent to both.

## LOC

Added ~20; removed 0. The next kind costs the same to write and stops being
possible to mis-spell in silence.

## Risk

Test-only. No production module, no wire field, no org bytes. It fails on the day
someone writes a kind literal inside a template string or builds one by
concatenation — which the regex will not see, and which the sweep guard turns into
a loud failure rather than a quiet pass.

## Precedent

`TestSelfContained.hs` already joins four such pairs: `jsconfig` against
`gluePartFiles` (`:71-76`), port names against the Elm declarations (`:142-153`),
one version across four files (`:175-184`), and a proposal's name against its own
header (`:79-96`). Its own comment states the rule: *"the second place each fact is
written"*.
