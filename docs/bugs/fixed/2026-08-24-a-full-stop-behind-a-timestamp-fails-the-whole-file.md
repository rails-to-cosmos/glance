# Bug — a full stop behind a timestamp fails the whole file

**Status:** fixed 2026-08-24 · **Reported:** 2026-08-24, by the user, off a
`glance scan ~/sync/views` run · **Surface:** `Data.Org.Parser`, and every
reader of the store behind it

## Symptom

A body sentence that closes a timestamp with punctuation —
`Research [2026-08-03 Mon]. Use case: music + podcasts, everyday.` — fails the
WHOLE document. `orgParse` is all-or-nothing (`Walk.hs:62`), so the file
contributes no rows: an org-glance registry blob's headline is absent from the
table, from every filter and from `ref:`.

The failure is silent everywhere except one command. The daemon counts it and
shows a reader nothing — `X-Glance-Parse-Failures` carries a NUMBER on the
store routes (`Routes.hs:875`) and no path — and "a parse failure fails exactly
one file" (`docs/invariants.md`) means the store keeps that file's previous
rows, which on a first load are none. Only `glance scan` names the file
(`Scan.hs:315`). Recorded as an observation; no fix owed for it here.

The reporting corpus answered 10 parse failures, 5 of them this class:

    .org-glance/data/56/d2eaf2-…/data.org: 11:26: unexpected '.'

Column 26 is the character immediately behind the timestamp's `]`.

## Steps to reproduce

A tree of one file, no `#+` headers, shaped like an org-glance registry blob:

    * Sennheiser HD 660S2
    :PROPERTIES:
    :ORG_GLANCE_ID: minimal-abut-fixture
    :END:
    Research [2026-08-03 Mon]. Use case: music + podcasts, everyday. Location: Haarlem, NL.

`glance scan DIR` before the fix:

    ok                        0
    parse failures            1
      …/data.org: 5:26: unexpected '.'

and `glance serve --dir DIR` answers `X-Glance-Rows: 0`. After the fix the same
tree scans clean and the daemon serves the row —
`{"id":"minimal-abut-fixture","cells":{"title":"Sennheiser HD 660S2",…}}`.

## Evidence

- `src/Data/Org/Parser.hs:49` — the top-level element loop. Between two
  elements the separator HAD to be whitespace: `takeWhile1P … isSpace`, or the
  loop stopped.
- `src/Data/Org/Parser.hs:102` — `elementP` tries `ETimestamp` ahead of
  `EToken`, so `[2026-08-03 Mon]` is claimed as a timestamp and the `.` behind
  it is left at a position the loop can neither continue from nor stop at.
- `src/Data/Org/Parser.hs:44` — `space <* eof` is where it dies, which is why
  the message names a line and column of the file and no element at all.
- `src/Data/Org/Parser.hs:93` — `spannedContainerUntil` separates its parts
  with `MPC.hspace`, which matches the empty run, so a TITLE holding the same
  sentence has always parsed. Only body text went through the
  whitespace-demanding loop. That asymmetry is why the suite stayed green:
  every timestamp case in `TestParser` and `TestRoundtrip` stood alone on its
  line or inside a title.
- `docs/invariants.md`, "A parse failure fails exactly one file"
  (`Edit.hs:169`, `Store.hs:210`) — the mechanism that scoped the damage is
  what hid it. One file's rows going missing looks exactly like a file with no
  rows.

The class was unpinned: nothing under `test/`, `docs/` or `AGENTS.hs` carried a
timestamp with a non-space character behind its closing bracket.

## Fix

`elementsP` takes a second separator. After a TIMESTAMP the next element may
abut it with nothing between, because a timestamp closes on its own bracket:
`[2026-08-03 Mon].` reads as a timestamp and the token `.`, org's own reading.

Only a timestamp is abuttable, and the abutting run is never at BOL:

- a token has already run to the next whitespace, so nothing can abut it;
- a HEADLINE that stops mid-line is corrupt org — `test/fixtures/broken/
  broken.org` is exactly that, `* A title with a :: double colon`, and
  `TestQuery.hs:877` pins it at one parse failure. Letting prose abut a
  headline would swallow that refusal;
- `bol` is `False` down the abutting branch, so a `*` behind a timestamp is
  ink, never a new row.

The range's `--` is untouched: `tsParser` reads it inside `MP.try`
(`Parser.hs:285`), so a real `[a]--[b]` is still one timestamp, and a `--` that
opens no second bracket is now the follower.

One consequence beyond the reported symptom: a MISMATCHED range,
`[2023-07-15 Sat 15:54]--<2023-07-15 Sat 17:10>`, no longer fails the file. It
degrades to the first timestamp and three tokens. The two cases that used it as
their refusal exemplar now use `broken.org`'s spelling — `TestNegative.hs:84`
and `TestGen.brokenLine` (`TestGen.hs:327`, read by
`TestProperties.hs:260`) — so the refusal path keeps a case and the degradation
gets one of its own.

Pinned by the follower truth table, `TestParser.hs:198`: `.` `,` `)` `:` `;`
`!` `?` a letter, the other bracket and `*`, active and inactive both, plus
line-end, end-of-document, two abutting timestamps, a `--` that opens no
timestamp, a real range wearing a follower, and the reported sentence under a
headline. Thirty cases, all red before the change.

## Not fixed here

- **A timestamp abutted on its LEFT is still not read.** `EToken` runs to the
  next whitespace, so `(<2026-08-03 Mon>)` is three tokens and the date inside
  the parens is invisible to every date column. It fails no file, which is why
  it stays outside this fix.
- **A planning line with prose behind its timestamp still fails.** `* T` /
  `SCHEDULED: <2026-08-03 Mon>.` ends the HEADLINE element mid-line, which is
  `broken.org`'s refusal rather than the abutting rule.
- **Five parse failures remain on the reporting corpus**, none of them this
  class: `1:11` and `1:16` and `1:25 unexpected ':'`, `23:32` and
  `1:160 unexpected ')'`. Those files were not opened — the corpus is the
  user's own data — so the class is read off the error shape alone: an element
  stopping mid-line with prose behind it, `broken.org` at corpus scale. They
  want their own bug.
