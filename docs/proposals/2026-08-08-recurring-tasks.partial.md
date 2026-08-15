# Proposal — recurring tasks

**Status:** partial — BUILT and tested, bar one rename.  `repeatOn`,
`shiftRepeat`, `repeatDay`, `Completion`, `completionLine` and `noteCompletion`
all shipped, wired through `Commands.writeOne` and covered by
`TestExternal.hs`'s "Completions" group and `TestSpec.hs`.  Decision 5 was
taken against the text below — `CLOSED:` is never shifted (`planStamps`), and
`:LAST_REPEAT:` was not written.  Only decision 6, the repeater-type rename,
is owed · **Date:** 2026-08-08

Repeating entries are the one org feature this daemon parses whole and
cannot act on: marking a repeating row DONE stops the repeat dead.  Take
org-clock's SHAPE — one append-only line per event — and move WHERE IT
LIVES: out of the entry, into a store-level ledger beside the two this
repo keeps already.

## What org does today

A repeater cookie rides a SCHEDULED or DEADLINE timestamp:
`<2026-08-08 Sat +1w>`.  Three kinds by prefix — `+N` shifts one
interval from the stamp, `++N` shifts by intervals until the stamp is
past today, `.+N` shifts N units from TODAY and forgets the stamp.
`org-todo` on such an entry runs `org-auto-repeat-maybe`: shift every
repeating planning timestamp forward, reset the keyword to the cycle's
first active state (or `org-todo-repeat-to-state`), write
`:LAST_REPEAT:`, log a state note into `:LOGBOOK:`.  The entry never
reaches DONE and leaves no completed row behind — the whole history is
the drawer.

**glance already parses all of it.**  `src/Data/Org/Types.hs` gives
`Timestamp` a `tsInterval :: Maybe TimestampRepeaterInterval` carrying
`repeaterType`, `repeaterValue`, `repeaterUnit` and `repeaterSign`, with
`tsWarning` beside it so a lone `-3d` stays org's warning cookie;
`src/Data/Org/Parser.hs`'s `tsRepeaterParser` reads the three prefixes
off `typeChar`, and `planningP` hangs the timestamp's own span on the
headline (`hsSchedule`, `hsDeadline`).  A cookie therefore survives
every write already — `setPlanningEdits` replaces the timestamp's span
alone and `planningTimestamp` keeps a bracketed value VERBATIM once it
reparses.  THE MISSING HALF IS BEHAVIOUR: nothing shifts, nothing
resets, nothing is recorded.  (Two of `TimestampRepeaterType`'s three
constructors read as their org opposites — decision 6.)

## What org-clock inspires, and what it gets wrong here

org-clock's mechanism is right: one line per event, append-only,
ordered, cheap to write, read by folding, never rewritten.  WHERE IT
LIVES is wrong here.  The lines go into a `:LOGBOOK:` drawer INSIDE the
entry, so the entry's bytes grow without bound, every read of the row
re-parses its whole history, that history cannot be read without reading
the file holding it, and the drawer sits between the headline and its
body, so span math around it moves on every completion.

This app already refuses to show that drawer: the subtree lens drops the
logbook whole (`hpLogbook`, server-preserved, re-injected verbatim by
`recomposedSubtree`) beside `hiddenProperties`, and the sheet draws it
as a read-only strip, out of Tab and out of `dirty()`.  THE STORAGE
OBJECTION, plainly: **A ROW'S HISTORY DOES NOT BELONG IN THE ROW** — the
growth is unbounded, per-row, and paid by every reader of the file.

## The ledger

The shape this repo uses twice already — `meta/EXTERNAL.jsonl`
(`Data.Org.External`) and org-glance's WAL folded by `Data.Org.Index`.
A third file joins them: **`<root>/.org-glance/meta/COMPLETIONS.jsonl`**.
`OCCURRENCES` is taken — `Data.Org.Walk.isOccurrence` names
`data/<id>/occurrences/<STAMP>.org`, a blob's own history the walk
DECLINES, and two things called occurrences under one `.org-glance` is a
bug waiting for a grep.

One JSON object per line, newline-terminated, field order frozen the way
`externalLine` freezes it (hand-assembled, values alone through the
encoder, where escaping has to happen):

    {"id":"e3b0…","at":"2026-08-08T09:12:44Z","state":"TODO","shifted":"<2026-08-15 Sat +1w>"}

- `id` — the row's `ORG_GLANCE_ID`.
- `at` — the server clock in UTC, `externalLine`'s own format.
- `state` — the keyword the entry MOVED TO, which is the reset target.
- `shifted` — the next occurrence as the file now spells it, verbatim,
  cookie included.

APPEND-ONLY, last-wins per `(id, at)` — `Data.Org.Index`'s fold rule one
key wider, which makes a retried write idempotent.  Written by
`Data.Org.External`'s own `appendLine`: `openFd` with `append` + `creat`
and ONE `fdWriteBuf`, because a `Handle` in `AppendMode` remembers the
offset it opened at and two writers then leave the file shorter than the
number of writes — a marked set across three files IS three threads, so
this is reachable rather than theoretical.  IO failures are swallowed
for `noteExternalWrite`'s reason: the org file is already renamed into
place, and failing the answer over a hint reports a landed write as one
that did not.

STORE-LEVEL.  The path is `metaDirIn (storeRootIn root)`, resolved as
`captureBlob` resolves its store, and **a tree with no `.org-glance`
keeps the org-native behaviour and no ledger** — the stamp shifts, the
keyword resets, nothing else is written, and no daemon makes a store
directory it was not given.  `meta` is on `Data.Org.Walk.isDerived`'s
denylist, so the file leaves the walk for free.

## THE LEDGER IS DERIVED, NEVER TRUTH

The org file keeps the repeater cookie and the CURRENT occurrence's
timestamp exactly as org writes them, so Emacs, `org-agenda` and every
other org tool read the entry correctly.  Delete `COMPLETIONS.jsonl` and
every entry is byte-identical with only the history gone; rebuild it and
no entry moves.  That is the invariant the feature adds, and what keeps
"org files are the single source of truth" standing.

## The command

`set-state` over a row whose SCHEDULED or DEADLINE carries a repeater
and whose named keyword is an INACTIVE one — org's own condition for a
repeat, the entry being closed.  ONE `replaceSpans` call composing three
edit sets: each repeating planning timestamp → its shifted text
(`setPlanningEdits`' "an entry already there is its own span" shape),
the keyword → the cycle's first ACTIVE state (`setStateEdits`' replace
arm), and `CLOSED:` set to now where the tree wants org's
`org-log-done` habit (decision 5).

They compose because `applyEdits` rejects only OVERLAP and the sets
touch disjoint spans — `rename-tag`'s own argument.  ONE write, ONE
digest, ONE inotify event, ONE frame, where two commands would be two
digests with the second drifting on the first.  The ledger line rides
`replaceSpans`' SUCCESS branch where `noteExternalWrite` already sits,
so `Watch.writeSpans` stays the one door and the path is nudged as ever.

**All three kinds in v1.**  `+N` adds one interval to the stamp, once,
so an entry three weeks overdue lands one week on and stays overdue —
org's behaviour, kept.  `++N` adds intervals until the stamp is past
today; `.+N` is today plus N.  The arithmetic is `Time.addDays` and
`Time.addGregorianMonthsClip`, both already in `planningTimestamp`'s
relative branch, and the `++` loop is the one new shape.  Shipping `+`
alone leaves two of the three cookies this corpus writes silently
ignored, the worse failure.  The TIME OF DAY rides through untouched
(`tsmHasTime` decides whether a time renders), as do the warning cookie
and a range end; the shifted stamp is `orgStamp`'s shape (day, COMPUTED
weekday, optional time), cookies re-rendered by `repeaterFormat` and
`warningFormat` in org's repeater-then-warning order.

WHICH KEYWORD IT RESETS TO is the row's own chain: `settableStates` is
`keywordSources` flattened, so the first active word of the widest scope
is the palette's own first entry and the reset agrees with what a reader
is shown.  A chain declaring no active keyword takes the state off.  A
row with no repeater takes the plain path unchanged, and the refusal
surface stays `set-state`'s.

## What the table shows

`repeats` on the row, SPARSE like `linked`: `rowJSON` emits
`"repeats": "+1w"` and nothing where the row has none, and SCHEMA.md's
Row being additive means no renderer edit is owed.  The value is
`repeaterFormat` over `tsInterval`, read at load off `hrHeadline`'s
`schedule` and `deadline` and `T.copy`-detached like every other cell —
SCHEDULED first, DEADLINE second, ONE value, since the field answers
whether the row comes back.  WIRE COST: a few bytes plus the key on a
repeating row, nothing on the rest — a few hundred over a full table of
this corpus, where repeating rows are a small minority.

NO `repeat:` FILTER KEY in v1.  `Glance.Web.Filter` dispatches on key
NAME, and every key it carries is a column key plus `planned` and `ref`;
a third is renderer-decidable only once `repeats` joins `hrSearch`, and
the parity contract holds the two matchers term for term.
`-planned:*empty*` already reaches every repeating row, a repeater
riding a planning timestamp by definition.  The key lands as one edit to
`viewKeys` and `VIEW_KEYS` when a reader asks for repeating rows.

THE STATE COLUMN SAYS NOTHING.  A repeating row's state is the keyword
the file spells, and a badge meaning "this comes back" is a second
vocabulary over one cell; `repeats` is where a renderer draws it, beside
the scheduled cell the cookie belongs to.

## Open decisions

1. **Per-store or per-root.**  PER-STORE: the served root's
   `.org-glance/meta`, resolved as `captureBlob` resolves it — one
   directory this daemon already writes into, one denylisted subtree,
   and a tree holding several stores gets a ledger each, the way
   `externalPathOf` gives it a note file each.  Per-root needs a
   directory nothing else owns and a rule for a nested store.

2. **A completion whose row has no id.**  DROPPED, no line.  `FILE#K` is
   the headline's place among its file's EMITTED rows and it moves — an
   insert ahead of it, a reorder, an entry going blank all renumber it —
   and `ORG_GLANCE_ID` is the only immunity, so a ledger keyed by an
   ordinal names a different row a week later.  `noteExternalWrite`
   keeps the rule already.  THE PRICE: a repeating entry outside the
   store records nothing and its write still lands.

3. **Read the ledger in v1?**  WRITE-ONLY.  No route serves it, no cell
   comes off it, no answer disagrees with a file because of it.  What it
   buys is a history existing from the first completion, so a reader
   added later has data.  What reading costs now: a fold per request or
   a second projection keyed by id, a second invalidation rule beside
   `stGen`/`stPrint`, and an answer going stale whenever Emacs completes
   an entry.  The shape when it comes is `GET /history?id=ROW`, the way
   `/links` answers about one row.

4. **Emacs completes the same entry.**  THE LEDGER IS INCOMPLETE BY
   CONSTRUCTION and no design here changes that: `org-todo` writes org's
   own LOGBOOK note and no ledger line, and the daemon sees the shifted
   stamp and reset keyword through the watch as an ordinary edit.  The
   ledger holds THIS DAEMON'S completions, the LOGBOOK holds Emacs's,
   and there is no join.  Say so wherever it is read — "completions
   recorded here" — rather than implying a total history.  Reading the
   LOGBOOK back means a second parser for org's log-note grammar: a fair
   proposal, and a different one.

5. **What else the write leaves in the file.**  `:LAST_REPEAT:` WRITTEN,
   since Emacs reads it and it is one property line, and added to
   `hiddenProperties` beside the id and the creation time so the lens
   neither shows it nor lets a client rewrite it — one edit, as that
   list promises.  `CLOSED:` where the tree's config asks for it, since
   every org tool reads a planning entry and the `closed` custom column
   serves it already.  NO state note: a drawer this app hides is a poor
   place for the one record it keeps.

6. **The repeater type names.**  Rename `Restart`, `CatchUp` and
   `Cumulative` to org's own cumulate (`+`), catch-up (`++`) and restart
   (`.+`).  The type is read by `typeChar`, `tsRepeaterParser` and the
   suite alone — three files, no wire, no config — and after this
   feature every reader of it does date math under a word meaning the
   opposite.
