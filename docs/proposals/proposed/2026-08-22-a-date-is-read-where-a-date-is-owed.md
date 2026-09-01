# Proposal — a date is read where a date is owed

**Status:** proposed · **Date:** 2026-08-22 · **Origin:** user — *"material doc
edit-commit parses human-readable English dates (yet — English only) into
org-mode dates, ACTIVE by default: `18 Aug` or `18 August` on RET becomes a
proper org active timestamp, current year filled in; case never matters
(August ≡ august ≡ AUGUST)."* The user points at python `dateutil` for a
reference corpus and invites better prior art from other worlds.

**Amended 2026-08-22, in review.** The user extends the grammar to English
INTERVALS — *"from 18 to 19 august"*, *"18 to 19 aug"* — and settles the two
points this document left open, verbatim:

1. **The output is org's own spelling:** `<2026-08-18 Tue>--<2026-08-19 Wed>`,
   the `--` pair, weekdays computed at both ends.
2. **An inverted range is refused.** Current-year-flat on both ends makes
   *"from 30 dec to 2 jan"* invert; the rule stays statable without a calendar
   by refusing end < start, and the typist spells a year to fix it
   (*"from 30 dec 2026 to 2 jan 2027"*).

## The law in one line

An English date phrase — one day, or an interval between two — becomes an org
ACTIVE stamp wherever the write already owes a date, and the half-typed bracket
asks for one where it does not: `18 Aug` in a planning value is
`<2026-08-18 Tue>`, `from 18 to 19 august` is
`<2026-08-18 Tue>--<2026-08-19 Wed>`, and `<18 aug>` in prose completes to the
first of those.

Two triggers, one parser, one renderer. The year defaults to the current one,
the month word is matched case-folded, and **the weekday in the answer is
COMPUTED** — the existing stamp law (`AGENTS.hs:3098`, *"ONE renderer, the
brackets the difference, the weekday COMPUTED"*). The interval costs no second
renderer: `TsMoment` carries **no weekday field** and recomputes it on render
(`AGENTS.hs:213`, *"recomputed from the date on render, so a locale's word costs
nothing"*), so both ends of a range get their weekday computed for free.

## The hazard this is designed around

A bare-phrase transform applied to committed prose is a false-positive machine.
`dateutil`'s own fuzzy mode is the demonstration, run locally against 3.14.7:

| typed                                 | `parser.parse(…, fuzzy=True)` |
|---------------------------------------|-------------------------------|
| `18 August was rainy`                 | 2026-08-18                    |
| `Chapter 18 Aug summary`              | 2026-08-18                    |
| `read pages 3 to 4`                   | 2026-03-04                    |
| `the March on Washington`             | 2026-03-01                    |
| `version 2.5 release`                 | 2026-01-02                    |
| `call me at 5`                        | 2026-01-05                    |
| `from 18 to 19 august`                | **2018**-08-19                |

The last row is the interval case and it is the worst of them: fuzzy scanning
reads the interval's START as a two-digit YEAR, drops the interval entirely, and
answers with one instant eight years off.

Every row is a sentence a reader might legitimately commit into a paragraph.
The design question is *where the transform fires*, and it has to be settled
before the grammar — a grammar of any size is safe in a field that owes a date
and dangerous in a field that holds prose.

## Three scopes, weighed

### (a) Date-owed contexts — RECOMMENDED

Three places where the write already means a date and nothing else can be
meant:

- **the planning line.** The sheet's meta row reads back through
  `Body.readPlanning` (`frontend/elm/src/Doc.elm:824`), rides the split write as
  `planning` (`:981`, `port docBody` at `:884`), and meets its wall in
  `badPlanning` (`src-web/Glance/Web/Routes.hs:514`), which today refuses any
  value `readsAsTimestamp` declines.
- **`set-planning`'s `date` argument** (`src-web/Glance/Web/Commands.hs:132`,
  resolved once per request at `:259` through `planningTimestamp`,
  `src-query/Glance/Query.hs:1545`). Its grammar is already
  `2026-08-05` / `today` / `tomorrow` / `+3d` / org's own brackets
  (`docs/commands.md:51`).
- **a date-shaped property value.** The drawer's pair rides the same split write
  as `properties` (`Doc.elm:981`) and is typed in the pane's new pair box
  (`frontend/glue/20-sheet.js:596` `pairRefused`, `:638`). "Date-shaped" needs a
  test that costs nothing: **a property whose value being REPLACED already
  reparses as a timestamp is a date property.** `readsAsTimestamp`
  (`Query.hs:970`) answers it, the old value is in hand at the wall, and no
  registry of date-property names has to exist or be configured.

In all three a bare `18 Aug` is unambiguous: the field's whole content is the
date, so there is no prose for the parser to misread. This is where the user's
*"ACTIVE by default"* applies without qualification.

### (b) The bracket trigger in free text — RECOMMENDED

Org already has an intent marker for "this is a date": the brackets. A committed
`<18 aug>` completes to `<2026-08-18 Tue>` and `[18 aug]` to the inactive twin
`[2026-08-18 Tue]`. **The half-typed bracket IS the ask.** Prose stays prose
because prose does not carry `<…>` around a day and a month by accident, and the
active/inactive choice is spelled by the reader rather than guessed by the
server — which is why (b) is what answers "how do I get the inactive one".

The trigger must NOT reuse the existing verbatim branch. `planningTimestamp`
keeps an already-bracketed value exactly as written once it reparses
(`Query.hs:1548`; `AGENTS.hs:3109` `verbatimDate Bracketed = True`), and
`test/TestQuery.hs:1791` pins the consequence: `<2026-08-05 Mon>` is written
through unchanged even though 2026-08-05 is a **Wed**. That law stands for org's
own spelling. A trigger is by definition not org's own spelling — it does not
reparse — so it falls to the parser and gets its weekday computed.

#### CALL: the trigger stays single-stamp, and composes into a range

**`<from 18 to 19 aug>` is refused; `<18 aug>--<19 aug>` is the range trigger,
and it needs no new law.** The trigger fires per BRACKET PAIR, so a reader who
writes two pairs joined by `--` gets two completions and the `--` they typed is
left alone — the answer is `<2026-08-18 Tue>--<2026-08-19 Wed>`, which is
exactly what the interval grammar produces elsewhere.

The grounds are the trigger's own premise. *The half-typed bracket is the ask*
works because the reader half-types the shape they want and the server fills it
in — input and output have the same shape. English inside org brackets breaks
that twice over: `<from 18 to 19 aug>` is one bracket pair that would have to
EXPAND into two, so the trigger stops completing and starts rewriting; and it
puts English where a reader reasonably expects org syntax. Composition keeps the
trigger a one-for-one completion and costs nothing to implement.

It also matches how org itself builds a range: `org-read-date` is called TWICE,
once per end, and the results are joined with `--`. The composed trigger is that
same gesture, typed.

**The inactive range twin lives in the composed trigger:** `[18 aug]--[19 aug]`
gives `[2026-08-18 Tue]--[2026-08-19 Wed]`. **A MIXED pair is refused** — left
as written — because the model cannot hold one: `Ts`'s bracket field is *"ONE
kind; both halves of a range share it"* (`AGENTS.hs:218`), so `<18 aug>--[19
aug]` names a timestamp that does not exist. In date-owed contexts the inactive
twin is asked for the same way, by spelling both bracket pairs; a bare
`from 18 to 19 aug` there is the ACTIVE range, per the origin. The planning line
is unaffected either way — SCHEDULED and DEADLINE are active by org convention,
and `CLOSED` is refused a write entirely (`AGENTS.hs:3105`
`settablePlan Closed = False`).

**English intervals are therefore owned by (a) and (c).** Free text still
reaches them two ways: the composed trigger above, and the whole-line rule
below.

### (c) The bare phrase in free text — RECOMMENDED under the whole-line rule

The user's literal example is `18 Aug` on RET. Applied to arbitrary committed
prose it costs exactly the table above. Applied under a **whole-line rule** —
*the committed line IS the date phrase, stripped, and nothing else* — the cost
collapses:

- `18 August was rainy` has four words; the grammar takes two or three. Left as
  text.
- `Chapter 18 Aug summary` likewise. Left as text.
- `read pages 3 to 4` carries the interval keyword and still fails: `pages` is
  no month and `read` is no `from`. Left as text. The interval grammar widens
  what a line may say without widening where a line is read.
- A line that is only `18 Aug` was almost certainly meant as a date, and the
  reader sees the answer immediately because the commit round-trips through the
  server and redraws (`commitDocEdit`, `frontend/glue/20-sheet.js:609` →
  `insertPara`/`editPara` → the model's own answer).

What it still costs: a paragraph consisting of the single line `18 August` — a
heading-ish note, a diary marker, a list item that is just a date — becomes a
stamp. That is a **visible, one-keystroke-undoable** change to a line the reader
just typed, not a silent rewrite buried in a paragraph. The whole-line rule is
what makes (c) shippable; without it (c) should be dropped.

**Recommendation: (a) + (b), plus (c) under the whole-line rule.** The user's
*active-by-default* rules the bare phrases in (a) and (c); the bracket pair in
(b) rules explicitly and is the only way to ask for the inactive twin.

## Prior art, weighed

**`org-read-date`** is the canonical reference for this job — it is the grammar
the target audience already has in its fingers. It accepts a bare day number
(`12` → the 12th of the current month), `feb 15` and `15 feb`, numeric
`3-2-5` forms, `+N` in org's shift charset, `++N` relative to the default date,
bare weekday words (`fri` → the next Friday), `.` for today, and bare times
(`12:45`). Its `org-read-date-prefer-future` bumps a reading that landed in the
past into the future. **Phase 1 takes exactly the day-and-month subset**
(`15 feb`, `feb 15`, either with a four-digit year). **Explicitly deferred:**
bare day numbers, bare month names, weekday words, `+3d`-style relatives beyond
the four `planningTimestamp` already reads, `.`, and times-of-day. Relatives and
weekday words are phase 2 at most — glance already has `today`, `tomorrow` and
`+N[dwmy]` on the planning path and that is the whole relative story it owes.
**On intervals `org-read-date` is single-instant**: it returns one date, and org
builds a range by calling it TWICE and joining the two stamps with `--` — which
is precisely the composed-trigger gesture recommended for (b) above.

**`dateutil` (python)** is the reference *corpus*, and it was run locally to
build the vector table below rather than quoted from memory. Its strict mode is
close to the right acceptance set and its failures are instructive: `18 Aug 18`
reads as **2018**-08-18 (two-digit year), `Thu 18 Aug` accepts a weekday word
and never checks it, `Aug` alone is Aug 1, `18` alone is Jan 18, `3/4/2026` picks
one of DD/MM and MM/DD by flag. Glance refuses all five. Its fuzzy mode is the
anti-pattern quantified above. **On intervals `dateutil` is single-instant too**
— `parse` returns one `datetime` and there is no range API at all, which is why
`from 18 to 19 august` is a `ParserError` strictly and the 2018 misreading
fuzzily.

**chrono-node (JS)** is the one reference here that genuinely parses spans: a
result carries `start` AND `end` components, so `"from 18 to 19 August"` comes
back as a single result with both ends filled — the output shape this proposal
wants. It reaches it by scanning arbitrary text and returning each match with
its index and matched span, alongside "Tuesday at 3pm" and "2 weeks from now".
**chrono-english / interim (Rust)** does the informal-English half: "next
friday", "8am tomorrow", "1 week ago". **Natty (Java)** is ANTLR-based and
returns `DateGroup`s carrying the matched text span, with holidays and
recurrence on top. All three are *scanners*: their contract is "find the dates
in this prose", which is precisely the contract glance must refuse. Their
span-returning API is the tell — a library that hands back *where in the
sentence* the date was found is built for a job glance does not have. What
glance borrows from chrono-node is the two-ended RESULT SHAPE; what it refuses
is the scan that finds it.

**No new Haskell dependency.** The grammar is a page; `time`'s
`parseTimeM`/`fromGregorianValid` do the validation, and `Query.hs` already
rolls the timestamp machinery by hand (`orgStamp` `:1591`, `timestampOf`
`:1269`, `readsAsTimestamp` `:970`).

## Grammar

### EBNF

```
phrase   ::= WS* ( date | span ) WS*
date     ::= day WS+ month [ WS+ year ]     -- 18 Aug · 18 August 2027
           | month WS+ day [ WS+ year ]     -- Aug 18 · August 18 2027
span     ::= [ "from" WS+ ] left WS+ "to" WS+ date
left     ::= day [ WS+ month [ WS+ year ] ] -- an elided field takes the right end's
day      ::= DIGIT | DIGIT DIGIT            -- 1..31, no ordinal suffix
year     ::= DIGIT DIGIT DIGIT DIGIT        -- four digits, never two
month    ::= <case-folded entry of the month table>
WS       ::= " " | "\t"

trigger  ::= "<" date ">"                   -- completes to the ACTIVE stamp
           | "[" date "]"                   -- completes to the INACTIVE twin
```

`WS+` is a RUN of whitespace, so `18  aug` is `18 aug`. The separator is
whitespace and nothing else — no `-`, no `/`, no `,`, no `.`.

`from` and `to` are case-folded like the month words, so `FROM 18 TO 19 AUG`
reads. `from` is optional and `to` is not: `18 to 19 aug` is the interval,
`18 19 aug` is text. The trigger takes `date` rather than `phrase` — the
interval is deliberately outside it, per the call in (b).

### The interval

**The left end inherits what it elides.** `left` may drop its month, or its
month and year together; each elided field takes the RIGHT end's value:

| typed                            | left       | right      |
|----------------------------------|------------|------------|
| `from 18 to 19 august`           | 2026-08-18 | 2026-08-19 |
| `18 to 19 aug`                   | 2026-08-18 | 2026-08-19 |
| `from 18 to 19 august 2027`      | 2027-08-18 | 2027-08-19 |
| `from 30 aug to 2 sep`           | 2026-08-30 | 2026-09-02 |
| `from 30 dec 2026 to 2 jan 2027` | 2026-12-30 | 2027-01-02 |

This is the English idiom — *"from 18 to 19 august"* says one month once — and
inheritance is the reading that keeps it honest. The alternative, defaulting the
left end's year independently to the current year, was rejected: it would read
`from 18 to 19 august 2027` as 2026-08-18 → 2027-08-19, a twelve-month span from
a phrase that plainly means two days. A refusal is always the better trade than
a wrong answer written silently to disk.

The cost is named: `from 30 dec to 2 jan 2027` inherits 2027 on the left, gives
2027-12-30 → 2027-01-02, inverts, and is refused. The typist spells both years.
That is the same remedy the inversion law already prescribes.

### Inversion

**An interval whose end falls before its start is REFUSED.** Current-year-flat
on both ends makes `from 30 dec to 2 jan` read 2026-12-30 → 2026-01-02, and the
refusal is what keeps the year rule statable without a calendar: there is no
"and if it would invert, roll the second end forward a year" clause to remember.
The typist spells a year — `from 30 dec 2026 to 2 jan 2027` — and gets what they
meant. The refusal shape per context follows the table below, unchanged.

The comparison is on MOMENTS, not on days, so it stays correct when times-of-day
arrive in a later phase.

#### CALL: the degenerate same-day interval COLLAPSES to one stamp

`from 18 to 18 aug` is `<2026-08-18 Tue>`, the single stamp — not a refusal and
not `<2026-08-18 Tue>--<2026-08-18 Tue>`.

Three grounds:

1. **The law stays "refuse end < start".** Refusing the equal case would make it
   "refuse end ≤ start", a second rule for a phrase that has an unambiguous
   meaning. A typist narrowing a range down to one day (`from 18 to 19` becomes
   `from 18 to 18`) should get the day, not a wall.
2. **With no times, `<D>--<D>` and `<D>` denote the same interval.** Emitting
   two stamps where one says it is the writer adding bytes the typist did not
   ask for, and every later read has to handle a degenerate end for nothing.
3. **It makes the grammar's two spellings agree.** `from 18 to 18 aug` and
   `18 aug` produce identical bytes, so the parser has one answer per meaning
   and idempotence holds across both spellings.

The collapse is tested on the two MOMENTS being equal rather than the two days,
which matters once times land: `from 18 aug 09:00 to 18 aug 11:00` shares a day
but not a moment, and is a genuine range — org's own COMPACT spelling
(`<D Tue 09:00-11:00>`), whose three conditions are the flag plus both ends
timed plus one day (`src/Data/Org/Types.hs:495`, `AGENTS.hs:235` `compactly`).
Stating the collapse on days would silently swallow that case in phase 2.

### The month table

Twelve entries, each with a three-letter form and a full form, matched after
`T.toLower`:

```
jan january · feb february · mar march  · apr april
may         · jun june     · jul july   · aug august
sep september · oct october · nov november · dec december
```

`may` is one entry: its short and full forms coincide. Four-letter `sept` is
**not** in the table (phase 2 if a reviewer wants it); neither is any
abbreviation carrying a full stop.

**Case-folding law:** the month word alone is case-folded, and the fold is
total — `August ≡ august ≡ AUGUST ≡ AuGuSt`. The precedent is in place:
`planningTimestamp` already folds `today`/`TODAY` (`Query.hs:1558`).

**The "March"-as-name-vs-verb question is a non-issue**, twice over. In a
date-owed context the whole field is the phrase, so there is no sentence for a
verb to live in; and a bare month name with no day is refused by the grammar
anyway, so `March` alone stays text everywhere.

### Year defaulting

**The current year, flat**, per the user's word: the reference day is the
server's own clock, read once per request (`resolveAsked`,
`Commands.hs:255`, *"ONE clock read, before any row: a marked set must not
cross midnight"*).

**Settled in review: current-year-flat, one rule, no per-context variation.**
`18 Aug` typed on 2026-12-30 means `<2026-08-18 Tue>` — the past — and that
is the rule as decided: the year is the clock's, stated without a calendar. A
typist who means next August writes the year (`18 Aug 2027`). Org's
`org-read-date-prefer-future` stays in Alternatives as the rejected reading.

### Validity

`Time.fromGregorianValid` is the wall: `31 Feb`, `29 Feb` in 2026, `31 Apr` and
`32 Aug` all fail it and none reaches the disk. A day outside 1..31 fails the
grammar first. Both ends of an interval take it independently, so
`from 18 aug to 31 feb` is refused on its right end.

### The output is org's own spelling: `--`

An interval renders as the **`--` pair** — `<2026-08-18 Tue>--<2026-08-19 Wed>`
— with the weekday computed at each end.

This is forced rather than chosen, which is worth spelling out because it means
no new renderer:

- The renderer already emits it. `TextShow Timestamp` writes
  `bracketed start <> "--" <> bracketed end` whenever `tsEnd` is set and the
  compact form does not apply (`src/Data/Org/Types.hs:487`).
- The compact form **cannot** apply here. It requires the flag AND both ends
  timed AND one day (`Types.hs:495`, `AGENTS.hs:235` `compactly`); phase 1 has
  no times, so a date interval is never compact and `--` is the only spelling
  available.
- Both ends share ONE bracket kind by construction (`AGENTS.hs:218`), so active
  and inactive intervals are `<A>--<B>` and `[A]--[B]` and nothing mixed exists.
- The weekdays are computed at both ends for free: `TsMoment` has no weekday
  field and recomputes on render (`AGENTS.hs:213`).
- **The wall already accepts it.** `test/TestQuery.hs:608` asserts
  `readsAsTimestamp "<2026-08-01 Sat>--<2026-08-05 Wed>"`, so a `--` interval
  passes `badPlanning` today, before any of this ships. The exact round-trip is
  pinned too: `test/TestRoundtrip.hs:36` carries
  `("Date range", "<2024-01-15 Mon>--<2024-01-19 Fri>", Exact)`.

The house's never-canonicalize pin (`AGENTS.hs:392`, *"The renderer never
canonicalizes a range: emacs writes CLOCK ranges as `--` though both halves
share a date"*) governs the neighbouring question and is cited here for its
boundary: it binds the RENDERER reading a file back, forbidding it to respell
what the source wrote. It does not choose a spelling for a stamp authored from
English — that is this section's job — and the two answers agree, since `--` is
what emacs writes and what glance will write.

#### Two range spellings, two layers, two laws

They must not be confused, and nothing in this proposal lets them meet:

| | the file format's | the filter grammar's |
|---|---|---|
| spelling | `<A>--<B>` | `A..B` |
| where it lives | bytes on disk | a `?q=` URL string |
| evidence | `Types.hs:487`, `TestRoundtrip.hs:36` | `AGENTS.hs:2400` `rangeMark = ".."`, `SRange` `:2407`, `docs/query.md:278` |
| when it is read | at parse, per file | at compile, once per request |

**`..` never appears in a file.** It is a read-side query token that compiles to
a predicate over cells (`stampMatch`, `AGENTS.hs:2449`) and is never written
anywhere. The English interval produces `--` and only `--`. A reader who types
`18 aug..19 aug` into a date-owed field gets no interval — `..` is not in the
grammar above — and the value is refused or left as text per its context.

**Two surfaces, one clock.** The filter grammar has since grown relative dates of
its own — `*today*` and the shift `BASE±N UNIT`, resolved at compile against the
request's day ([`docs/query.md`](../../query.md#a-date-can-be-shifted)) — so both
sides of the tree now spell "thirty days out", each against the same
once-per-request read this proposal takes at `Commands.hs:255`, and neither in
the other's syntax: the filter's shift compiles to a predicate and disappears,
where this proposal's English resolves to bytes org itself would write.

### Idempotence

A value that already reparses as an org stamp passes through unchanged — the
existing bracketed branch (`Query.hs:1548`). So a commit is idempotent: RET,
RET, RET on the same field yields the same bytes, which is what
`docs/invariants.md:69` (*"anything written back must reparse under the
parser's own charsets"*) needs to stay true across repeats.

Intervals inherit this whole: a produced `<A>--<B>` reparses
(`TestQuery.hs:608`) and round-trips exactly (`TestRoundtrip.hs:36`), so the
second RET is a no-op like the first. The degenerate collapse is what keeps this
true across SPELLINGS as well as repeats — `from 18 to 18 aug` and `18 aug`
land on the same bytes, so neither can be a fixed point the other lacks.

## The vector table

Reference day **2026-08-22 (Sat)**; current year **2026**. The `dateutil`
column is what python 3.14.7's `dateutil.parser.parse(…, default=2026-01-01)`
actually returned, run locally.

| typed                    | dateutil     | glance                | why                                          |
|--------------------------|--------------|-----------------------|----------------------------------------------|
| `18 Aug`                 | 2026-08-18   | `<2026-08-18 Tue>`    | the ask                                      |
| `18 August`              | 2026-08-18   | `<2026-08-18 Tue>`    | full form                                    |
| `18 august`              | 2026-08-18   | `<2026-08-18 Tue>`    | case never matters                           |
| `18 AUGUST`              | 2026-08-18   | `<2026-08-18 Tue>`    | case never matters                           |
| `AUGUST 18`              | 2026-08-18   | `<2026-08-18 Tue>`    | month first                                  |
| `aug 18`                 | 2026-08-18   | `<2026-08-18 Tue>`    | month first, short form                      |
| `18  aug`                | 2026-08-18   | `<2026-08-18 Tue>`    | a whitespace RUN is one separator            |
| `  18 aug  `             | 2026-08-18   | `<2026-08-18 Tue>`    | stripped, today's law                        |
| `18 aug 2027`            | 2027-08-18   | `<2027-08-18 Wed>`    | the year overrides the default                |
| `aug 18 2027`            | 2027-08-18   | `<2027-08-18 Wed>`    | both arrangements take a year                |
| `18 Sep`                 | 2026-09-18   | `<2026-09-18 Fri>`    | the weekday is COMPUTED per date             |
| `1 Mar`                  | 2026-03-01   | `<2026-03-01 Sun>`    | one-digit day                                |
| `1 May`                  | 2026-05-01   | `<2026-05-01 Fri>`    | `may` is one table entry                     |
| `May 1`                  | 2026-05-01   | `<2026-05-01 Fri>`    | and both ways round                          |
| `2026-08-18`             | 2026-08-18   | `<2026-08-18 Tue>`    | ISO passthrough, unchanged law               |
| `<2026-08-18 Tue>`       | —            | `<2026-08-18 Tue>`    | idempotence                                  |
| `<2026-08-18 Mon>`       | —            | `<2026-08-18 Mon>`    | org's own kept VERBATIM (`TestQuery.hs:1791`) |
| `[2026-08-18 Tue]`       | —            | `[2026-08-18 Tue]`    | the inactive twin, unchanged                 |
| `31 Feb`                 | ParserError  | refused               | `fromGregorianValid`                         |
| `29 Feb`                 | ParserError  | refused               | 2026 is no leap year                         |
| `31 Apr`                 | ParserError  | refused               | April has thirty days                        |
| `18 Bug`                 | ParserError  | left as text          | no month by that name                        |
| `18 Augustus`            | ParserError  | left as text          | the table is exact                           |
| `Sept 18`                | 2026-09-18   | left as text          | four letters; three-letter and full only     |
| `Sep. 18`                | 2026-09-18   | left as text          | no full stop in the grammar                  |
| `18-Aug-2027`            | 2027-08-18   | left as text          | the separator is whitespace                  |
| `August 18, 2027`        | 2027-08-18   | left as text          | the comma is phase 2                         |
| `18 Aug 27`              | 2027-08-18   | left as text          | a year is four digits                        |
| `18 Aug 18`              | **2018**-08-18 | left as text        | dateutil's worst reading                     |
| `Aug`                    | 2026-08-01   | left as text          | a bare month is no date                      |
| `March`                  | 2026-03-01   | left as text          | the verb question never arises               |
| `18`                     | 2026-01-18   | left as text          | a bare day is no date                        |
| `3/4/2026`               | 2026-03-04   | left as text          | DD/MM vs MM/DD has no answer                 |
| `Thu 18 Aug`             | 2026-08-18   | left as text          | a weekday is computed, never read            |
| `Tue 18 Aug`             | 2026-08-18   | left as text          | even a CORRECT weekday                       |
| `18th Aug`               | 2026-08-18   | left as text          | the ordinal suffix is phase 2                |
| `18 August was rainy`    | 2026-08-18 (fuzzy) | left as text    | the whole field is the phrase                |
| `Chapter 18 Aug summary` | 2026-08-18 (fuzzy) | left as text    | the whole-line rule                          |
| `read pages 3 to 4`      | 2026-03-04 (fuzzy) | left as text    | what fuzzy costs                             |
| `<18 aug>`               | —            | `<2026-08-18 Tue>`    | the bracket trigger                          |
| `[18 aug]`               | —            | `[2026-08-18 Tue]`    | the inactive twin, asked for                 |
| `<18 august 2027>`       | —            | `<2027-08-18 Wed>`    | the trigger takes the whole DATE grammar     |
| `<AUGUST 18>`            | —            | `<2026-08-18 Tue>`    | case folds inside the trigger too            |
| `<31 feb>`               | —            | left as written       | a trigger naming no day stays typed          |
| `<18 aug 09:30>`         | —            | left as written       | times-of-day are phase 2                     |
| `today`                  | ParserError  | `<2026-08-22 Sat>`    | planning path only, unchanged                |
| `+3d`                    | ParserError  | `<2026-08-25 Tue>`    | planning path only, unchanged                |
| `18 Aug` typed 2026-12-30 | 2026-08-18  | `<2026-08-18 Tue>`    | the settled rule: the year is the clock's    |

### Intervals

Same reference day. `dateutil` has **no range API at all**, so its column is its
strict answer, with the fuzzy answer noted where it differs — every one of them
a single instant, which is the point.

| typed                            | dateutil                     | glance                                | why                                       |
|----------------------------------|------------------------------|---------------------------------------|-------------------------------------------|
| `from 18 to 19 august`           | ParserError (fuzzy **2018**-08-19) | `<2026-08-18 Tue>--<2026-08-19 Wed>` | the user's first form                     |
| `18 to 19 aug`                   | ParserError (fuzzy **2018**-08-19) | `<2026-08-18 Tue>--<2026-08-19 Wed>` | the user's second — `from` is optional    |
| `FROM 18 TO 19 AUG`              | ParserError (fuzzy **2018**-08-19) | `<2026-08-18 Tue>--<2026-08-19 Wed>` | `from`/`to` fold like the months          |
| `from 18 aug to 19 aug`          | ParserError                  | `<2026-08-18 Tue>--<2026-08-19 Wed>`  | the left end may spell its own month      |
| `from 18 to 19 august 2027`      | ParserError (fuzzy **2018**-08-19) | `<2027-08-18 Wed>--<2027-08-19 Thu>` | the left inherits month AND year          |
| `from 30 aug to 2 sep`           | ParserError                  | `<2026-08-30 Sun>--<2026-09-02 Wed>`  | cross-month, each end its own             |
| `from 30 dec 2026 to 2 jan 2027` | ParserError                  | `<2026-12-30 Wed>--<2027-01-02 Sat>`  | both years spelled — the invert's remedy  |
| `from 18 to 18 aug`              | ParserError (fuzzy **2018**-08-18) | `<2026-08-18 Tue>`             | the degenerate case COLLAPSES             |
| `from 30 dec to 2 jan`           | ParserError                  | refused                               | inverted under current-year-flat          |
| `from 30 dec to 2 jan 2027`      | ParserError                  | refused                               | the left inherits 2027 and inverts        |
| `from 18 aug to 31 feb`          | ParserError                  | refused                               | `fromGregorianValid` on the right end     |
| `18 19 aug`                      | **2018**-08-19               | left as text                          | `to` is not optional — and dateutil's misread |
| `18 aug..19 aug`                 | ParserError                  | left as text                          | `..` is the FILTER's range, never a file's |
| `<18 aug>--<19 aug>`             | —                            | `<2026-08-18 Tue>--<2026-08-19 Wed>`  | two triggers COMPOSE, the `--` left alone |
| `[18 aug]--[19 aug]`             | —                            | `[2026-08-18 Tue]--[2026-08-19 Wed]`  | the inactive interval                     |
| `<18 aug>--[19 aug]`             | —                            | left as written                       | ONE bracket kind per range (`AGENTS.hs:218`) |
| `<from 18 to 19 aug>`            | —                            | left as written                       | the trigger is single-stamp, per the call |
| `<2026-08-18 Tue>--<2026-08-19 Wed>` | —                        | unchanged                             | idempotence (`TestQuery.hs:608`)          |

**48 single-date rows + 18 interval rows = 66 rows.**

## Refusal shapes, per context

The parser has one answer — a day, or a pair of days — and each context spends
it its own way. This is the part a reviewer should read twice, because "refused"
means three different things:

| context                | a phrase that parses    | a phrase that does not                             |
|------------------------|-------------------------|----------------------------------------------------|
| `set-planning` `date`  | the stamp is written    | today's 400 naming the accepted spellings (`Commands.hs:259`) |
| the planning line      | the value is REWRITTEN  | today's 409 `{error, reason: "planning", field}` (`Routes.hs:503`) |
| a date-shaped property | the value is REWRITTEN  | written as typed — a property takes any text        |
| a bracket trigger      | completed to the stamp  | left exactly as written — `<18 Bug>` is prose        |
| a whole-line paragraph | replaced by the stamp   | left exactly as written                             |

**An inverted interval takes the same column.** `from 30 dec to 2 jan` is a
phrase that does not parse, so it 400s under `set-planning`, 409s on the
planning line, is written as typed into a property, and is left alone in prose —
no new refusal machinery, and the sentence naming the accepted spellings
(`Query.hs:1553`) gains the interval forms alongside the rest. The inversion is
worth its own WORDING in that sentence, since "not a date" reads oddly for a
phrase that names two perfectly good ones: it should say the end falls before
the start and that spelling a year fixes it.

The two walls that 4xx are walls **today**, for values that would stop being
planning entries on the next parse (`docs/invariants.md:69`). Nothing in this
proposal makes a previously-accepted value fail; the parser only widens what
passes — and the interval's own output was already passing before it
(`TestQuery.hs:608`).

**One shape change is real and must be named.** `badPlanning` is today a
pure predicate — `Commitment -> Maybe Text` (`Routes.hs:514`). Rewriting a
planning value means `prepare` (`:498`) gains a NORMALIZING step ahead of
`committed` (`:508`), so the function becomes `Commitment -> Either Text
Commitment`. Same call site, same 409, one more return arm. The `WholeSubtree`
arm stays untouched: the raw `mtext` half of the sheet
(`frontend/glue/20-sheet.js:913`, `asked()` sending `{org: …}`) hands back a
whole document the client typed, and rewriting bytes inside it would be the
server editing a text editor's buffer. **The raw half transforms nothing** —
that asymmetry is deliberate and should be stated in the docs.

## Where the parser lives

**Server-side, at the write walls, in `src-query/Glance/Query.hs` beside the
existing timestamp machinery.** The pane and the renderer never grow one.

This is forced rather than chosen. `AGENTS.hs:4547` — *"The page holds no org
parser and must not grow one"* — and the same law spelled at the point of
temptation in `frontend/glue/20-sheet.js:231`: *"a second seeding would
overwrite the typing, and this page spells no org."* A client-side transform
would also have to be a SECOND implementation, since `set-planning` reaches the
same grammar over `POST /command` with no page involved.

The round-trip is what makes the transform visible: the doc pane already
commits through the server and redraws off the model's own answer
(`commitDocEdit` `:609` → `insertPara` `:271` → the write's answer), so the
reader sees `<2026-08-18 Tue>` appear one turn after RET the same way they see
every other write land.

## Implementation sketch

- **`src-query/Glance/Query.hs`** — one new exported function beside
  `planningTimestamp`:

  ```haskell
  englishDay  :: Time.Day -> Text -> Maybe Time.Day                 -- ~45 lines with the table
  englishSpan :: Time.Day -> Text -> Maybe (Time.Day, Time.Day)     -- ~25 more
  ```

  `englishDay` is the month table (12 pairs), a whitespace split, the two
  arrangements, and `fromGregorianValid`. `englishSpan` splits on the `to`
  keyword, parses the right end with `englishDay`, parses the left end against
  the right's month and year as defaults, and refuses `end < start`; the
  degenerate equal-ends case returns the single day so the caller renders one
  stamp. `planningTimestamp` (`:1546`) gains both in its `dated` chain — the
  span tried first, since `to` makes the phrase unambiguous — which alone
  delivers `set-planning`.

  The interval needs **no new renderer**: `orgStamp` (`:1591`) renders each end
  and the `--` is a literal between them, or equivalently the existing
  `TextShow Timestamp` path (`src/Data/Org/Types.hs:487`) does it whole. Either
  way `activeBrackets`/`inactiveBrackets` (`:1581`) stay the only bracket
  source, so there is exactly ONE renderer and the weekday is computed at both
  ends. A second small function completes a bracket trigger — per pair, so the
  composed `<18 aug>--<19 aug>` falls out with no interval code in it.
- **`src-web/Glance/Web/Routes.hs`** — `badPlanning` (`:514`) becomes the
  normalizer described above; `prepare` (`:498`) threads it. The property arm
  reads the row's existing pair, tests it with `readsAsTimestamp` (`:970`), and
  rewrites only when it answered `True`.
- **`src-web/Glance/Web/Commands.hs`** — nothing, once `planningTimestamp`
  widens; the refusal sentence at `Query.hs:1553` gains the English forms so
  the 400 keeps naming everything accepted (`TestQuery.hs:1818` already pins
  that the refusal enumerates the relative forms — the English forms want the
  same treatment).
- **`AGENTS.hs`** — `DateForm` (`:3107`) gains `English` and `EnglishSpan`
  constructors, both with `verbatimDate _ = False`, which is the spec saying the
  weekday is computed for them. One `Note` in `queryNotes` for the whole-line
  rule and one for the two range spellings — `--` is the file's, `..`
  (`:2400`) is the filter's, and neither crosses.
- **Tests.** `TestQuery` takes the vector table above as a `reads'`/`refuses`
  group beside *"the dates `set-planning` takes"* (`TestQuery.hs:1785`) — the
  66 rows are already in `reads'`/`refuses` shape. Four intervals want their own
  assertions: the degenerate collapse yields ONE stamp, the inverted pair
  refuses, the produced `--` pair satisfies `readsAsTimestamp` (the existing
  *"everything it computes reads back as a timestamp"* case at `:1808` extends
  to it), and `from 18 to 19 august` equals `<18 aug>--<19 aug>` byte for byte.
  `TestServe` takes one drive per context: the planning line rewritten, a
  date-shaped property rewritten, a non-date property left alone, the raw
  `WholeSubtree` half left alone, the 409 unchanged for `31 Feb`, and one for
  the inverted interval. `test/browser` takes one case per surface: the planning
  row, the pair box, a `<18 aug>` paragraph, and one `from 18 to 19 august`
  whole line.
- **Docs.** `docs/commands.md`'s `## Dates` (`:49`) gains the English forms, the
  interval forms and the current-year rule; the sheet's half wants a short
  section naming the triggers, the whole-line rule, the raw-half exemption and
  the `--`/`..` distinction. `docs/query.md:278` (`A..B`) wants one sentence
  saying the filter's range is a query token and never a file's bytes.
- **LOC.** ~45 lines of date parser, ~25 of interval parser, ~15 of trigger
  completion, ~20 of wall rewiring, ~170 of tests.

### Phases

1. **The grammar and the date-owed contexts** — `englishDay`, `englishSpan`,
   `set-planning`, the planning line, date-shaped properties. Self-contained and
   shippable. The interval rides phase 1 rather than trailing it: it shares the
   parser, the renderer and the walls, and splitting it out would mean shipping
   a refusal for `from 18 to 19 august` and then withdrawing it.
2. **The bracket trigger** — free text, both bracket kinds, one completion per
   pair. Depends on nothing in phase 1 but reuses its parser, and gets composed
   intervals for free.
3. **The whole-line bare phrase** — the smallest surface and the one the review
   is most likely to want to see running before agreeing to. Intervals reach
   free text here.

### What this deliberately does not do

Relative words (`next tuesday`, `fri`, `in 2 weeks`), times-of-day inside an
English phrase (and so org's COMPACT range spelling), ordinal suffixes,
non-whitespace separators, two-digit years, open-ended intervals (`from 18 aug`
with no end), `between … and …` as an interval keyword, locales beyond English,
and prose scanning of any kind. The origin's *"yet"* is the extension point and
it is named here: **the month table is the only language-bearing datum**, so a
second language is a second table and a selector — plus `from`/`to`, which the
same table can carry — and nothing else in the parser moves.

## Alternatives considered

- **`dateutil`-style fuzzy scanning of committed prose.** Rejected: the table
  at the top of this document is the reason, seven rows of ordinary English that
  become dates. A transform whose false-positive rate depends on what the
  reader happens to be writing about cannot be on by default. The interval row
  sharpens it — fuzzy scanning turns `from 18 to 19 august` into a single
  instant in **2018**, so it is wrong about the year, the day AND the arity.
- **A client-side parser in the sheet.** Rejected twice: `AGENTS.hs:4547` and
  `20-sheet.js:231` forbid the page holding an org parser, and `set-planning`
  would need a second implementation anyway since it never touches the page.
- **A new Haskell dependency for date parsing.** Rejected: the accepted grammar
  is the EBNF block above, `time` already validates, and `Query.hs` rolls the
  rest of the timestamp machinery by hand. A dependency here buys the fuzzy
  behaviour this proposal is built to refuse.
- **A configured list of date-shaped property names.** Rejected in favour of
  "the value being replaced already reparses as a timestamp": no config surface,
  no registry to drift, and the answer is right by construction for
  `ORG_GLANCE_CREATION_TIME`-shaped properties.
- **Prefer-future year defaulting.** Rejected in review: the year is the
  clock's, flat — a typist meaning next year writes the year.
- **Rolling an inverted interval's end forward a year** (`from 30 dec to 2 jan`
  becoming 2026-12-30 → 2027-01-02). Rejected in review: it is exactly the
  calendar-dependent clause the flat year rule exists to avoid, and it would
  make the interval's year rule differ from the single date's. The refusal is
  the honest answer and the remedy is one typed year.
- **Defaulting the interval's left-end year independently** rather than
  inheriting the right's. Rejected: it reads `from 18 to 19 august 2027` as a
  twelve-month span, which is a silent wrong answer where inheritance gives
  either the right one or a refusal.
- **Refusing the degenerate `from 18 to 18 aug`.** Rejected: it needs a second
  validity rule (`end ≤ start`), and the phrase has one obvious meaning that
  the collapse delivers. See the call above.
- **An English interval inside one bracket trigger** (`<from 18 to 19 aug>`).
  Rejected: one bracket pair would have to expand into two, so the trigger stops
  being a completion. `<18 aug>--<19 aug>` composes instead, which is also how
  org builds a range with `org-read-date`.
- **Emitting the filter grammar's `..` into a file.** Never considered
  seriously, listed so the boundary is on the record: `..` compiles to a
  predicate at request time (`AGENTS.hs:2400`, `:2449`) and no writer produces
  it. The file format's range is `--`.

## As delivered (the planning slice)

Delivered: the PLANNING half of context **(a)** — `set-planning`'s `date` and
the planning line's own wall — for **SCHEDULED and DEADLINE**, with a date
WIDGET in the material document standing where the value stands. The grammar
shipped as this document settles it; the contexts shipped narrower than the
recommendation, and what is left out is named below rather than assumed.

- **The grammar, whole and as reviewed.** Both arrangements with an optional
  four-digit year, the exact month table folded totally, current-year-flat (a
  typist meaning next year writes the year), a bare day and a bare month
  refused, the weekday COMPUTED and never read — org's own bracket the one
  verbatim exception, wrong weekday and all (`test/TestQuery.hs:1791`) —
  whitespace the only separator, `from` optional and `to` not, the left end
  inheriting month and year, an inverted range refused in words naming the end
  as falling before the start, the degenerate same-day range collapsing to one
  stamp, and `--` the only spelling a range is written in. The vector table
  above is the corpus, and it is the corpus TWICE — see the two-resolver law
  below.

- **The wall grew the transform the shape change named.** `badPlanning`
  (`src-web/Glance/Web/Routes.hs:523`) stops being a predicate: it answers the
  refusal or the value transformed, and `prepare` (`:507`) threads it ahead of
  `committed` (`:517`). `planningTimestamp` (`src-query/Glance/Query.hs:1633`)
  widened in place, so `POST /command` grows no second reader and its 400 keeps
  naming everything accepted. **The raw half transforms nothing** — the
  `WholeSubtree` arm is untouched, the asymmetry this document asked to have
  stated, and `docs/commands.md` states it.

- **The date widget — the delivery's own addition, settled by the spike**
  ([five places for org-calendar's job](../../../spikes/2026-08-23-date-widget/README.md),
  whose D is the shipped shape). `C-c C-s` and `C-c C-d` in the material
  document raise a FIELD in the value's own slot — the planning line's, the
  line drawn in if the row has none — where they raised a blind `askText`
  prompt before. ONE LINE: the value as typed, and the resolver's preview
  riding after it as GHOST — `10 jan → <2026-01-10 Sat>` in the mute ink, a
  refusal no further character can rescue in the marked ink, and nothing at
  all over an empty field, over a term still being written, or over a value
  that is already its own answer. The field wears the pane's own editing
  dress wholesale — the pair box's font, ground, accent focus and
  `::selection` — and invents nothing. Opening over a standing value selects
  it WHOLE, so one keystroke replaces it and a bare `RET` recommits it byte
  for byte, the way `org-read-date` takes its default; `RET` is dry over an
  offer and final over a completed value; `ESC` cancels the input whole and
  byte-identical; an empty commit clears the entry, the shipped promise kept;
  `S-←`/`S-→` and `S-↑`/`S-↓` adjust a day and a week in place with the ghost
  following. The pair box's value half, where its key routes to SCHEDULED or
  DEADLINE, wears the same field and the same ghost: one widget, both doors.

- **The two-resolver law, which is where this document's own rejection needs
  reading twice.** *A client-side parser in the sheet* stays rejected for the
  WRITE, and the delivery is that rejection kept: the commit sends the RAW
  typed text, the server transforms it at the wall against its own clock, and
  the pane redraws off the server's answer — the truth, as ever. What the
  ghost owes is a preview BEFORE the commit, which is the whole complaint
  against the blind prompt (a refusal that arrives after the box has shut has
  nothing left on screen to fix), so the client carries a SECOND resolver
  whose only output is ink. It writes nothing and it is never the value. The
  two are **DRIFT-PINNED over one shared corpus** — the vector table above,
  read by both sides, the house pattern the planning wall's spellings already
  live by — and where they part the server's answer is what the reader ends up
  looking at.

- **What stays proposed.** The rest of context (a) — CLOSED and date-shaped
  custom properties — and the whole of (b) and (c). CLOSED is refused a
  planning write entirely (`AGENTS.hs:3422` `settablePlan Closed = False`), so
  its half of the question is the drawer pair alone; a date-shaped property
  still wants the test on the value being REPLACED, `readsAsTimestamp`
  (`src-query/Glance/Query.hs:1040` — the `:970` cited above has drifted).
  The bracket trigger in prose and the whole-line rule are untouched: **a bare
  phrase in free text is not delivered**, and `18 August` committed into a
  paragraph is the text it always was.

The property half of (a), and phases 2 and 3, are inert until reviewed.
