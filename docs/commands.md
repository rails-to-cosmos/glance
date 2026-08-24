# The command API

Every write the page makes goes through one door, and the door is public:

```
POST /command  {"name": …, "id": … | "ids": […], "args": {…}, "digests": {…}}
```

The README's table is the crib; this page is the whole law. The registry is
`Glance.Web.Commands.commands` — one entry per name, each with its own
argument wall and its own splice.

## The laws every command shares

- **Ids batch by file, one drift-locked atomic write per file.** No rollback
  across files: each file's write stands or refuses on its own, and the
  answer is **per id**, in the order the request named them.
- **A shape refusal is the whole request's** — a missing argument, an unknown
  name, a malformed date is HTTP 400 with nothing written anywhere. A
  per-row failure (an unknown id, a drifted file) refuses that row inside a
  200: `{"id": …, "ok": false, "why": …}`.
- **Digests pin the write.** `digests` maps each id to the file digest the
  client last saw; a file whose bytes moved since answers
  `…changed on disk (it digests to …); nothing was written to it` — the
  stale-tab wall. A success answers `{"id": …, "ok": true, "file": …,
  "digest": <fresh>}`.
- **The route never touches the store.** Rows come back over the file
  watch — the response tells you what was written, the next push tells you
  what it now looks like.

## The commands

| name | args | writes |
| --- | --- | --- |
| `set-state` | `{keyword}`, null clears | the keyword span. A repeater task answers a done-keyword by **shifting its date** instead, org's own repeat, and records the turn in `:LOGBOOK:` — the one command that records anything |
| `set-priority` | `{priority}`, null clears | org's `[#A]` token |
| `set-title` | `{title}` | the title span alone |
| `set-planning` | `{keyword, date}`, null date clears | one planning entry — `SCHEDULED`, `DEADLINE` or `CLOSED`; the first two read the grammar below, `CLOSED` takes org's own bracket **verbatim or not at all** |
| `add-tag` / `remove-tag` | `{tag}` | the tag on / off the headline |
| `rename-tag` | `{from, to}` | both spellings, one write per file |
| `archive` | `{}` | `add-tag ARCHIVE`; idempotent |
| `capture` | `{text, tag?, fields?}` | a headline in the inbox, or a minted store blob — the whole flow is [capture.md](capture.md) |
| `edit-link` | `{span, target, desc?}` | one link's own character range |
| `delete` | `{}` | the row's blob, gzipped into the store's `trash/` |

Tags and titles pass the same charset walls the page enforces — padding is
refused, so the string tested is the string written.

## Dates

`set-planning`'s `date` (and a repeat's arithmetic) reads one grammar,
resolved against the server's clock **once per request** — a marked set
must not cross midnight.

**The keyword picks the wall.** `SCHEDULED` and `DEADLINE` are the two this
server *composes* a value for: they read the whole grammar below and the answer
is the bytes org itself would write. `CLOSED` is org's own bookkeeping and is
never resolved for — it takes a timestamp that **reparses**, verbatim, and every
other spelling is a 400 saying `CLOSED is not a timestamp org would read back`.
A keyword naming no planning entry is refused ahead of the date: an unknown key
outranks every value. `POST /headline` splits the same way, so a value one door
takes is a value the other takes.

The grammar the two composed keys read:

- `2026-08-05`, `2026-08-05 09:30`
- `today`, `tomorrow` — the DAY WORDS, which the filter grammar reads too, off
  the one base reader; `*today*` is `today`'s old spelling and is read beside them
- `+3d` and friends — org's own shift charset (`d`, `w`, `m`, `y`) — bare, or
  on either word: `today+30d`, `tomorrow+1w`
- `18 aug`, `aug 18`, `18 August 2029` — the English day-and-month forms
- `from 18 to 19 august`, `18 to 19 aug` — the English interval, written as
  org's own `--` pair with the weekday computed at both ends
- org's own `<2026-08-05 Wed>` — kept **verbatim** when it reparses

Anything else is a 400 naming the accepted spellings. Rendered stamps get
their weekday computed for you.

### The English forms

The whole field is the phrase, so a bare `18 aug` is unambiguous: this
grammar is read where a date is owed and nowhere else, never over prose.

- **The year is the clock's, flat.** `18 aug` typed on 2026-12-30 is
  `<2026-08-18 Tue>`, the past. A typist meaning next August writes the year.
- **A year is four digits.** `18 aug 27` is refused, never read as 2027.
- **The month word folds totally** — `August ≡ august ≡ AUGUST` — over an
  exact table: twelve short forms and twelve full ones, no `sept`, no form
  carrying a full stop.
- **A bare day and a bare month are no date.** `18` and `March` are refused,
  which is also why the name-versus-verb question never arises.
- **A weekday is COMPUTED, never read.** `Thu 18 aug` is refused even when the
  weekday is right. Org's own bracket is the one exception — it is kept
  verbatim wherever it reparses, wrong weekday and all.
- **The separator is whitespace, and a run of it is one.** `18  aug` reads;
  `18-Aug-2027`, `August 18, 2027` and `18th aug` are refused.
- **`from` is optional and `to` is not.** `18 19 aug` names no interval.
- **The left end inherits what it elides**, month and year both:
  `from 18 to 19 august 2027` is 2027 at each end.
- **An interval whose end falls before its start is REFUSED.**
  `from 30 dec to 2 jan` reads 2026-12-30 → 2026-01-02 under the flat year,
  and the refusal says the end falls before the start; the typist spells both
  years (`from 30 dec 2026 to 2 jan 2027`).
- **The degenerate interval collapses.** `from 18 to 18 aug` is the single
  stamp `<2026-08-18 Tue>` — the bytes `18 aug` writes, so one meaning has one
  spelling.
- **The filter's date syntax stays the filter's.** `>=`, `<=` and the range
  `A..B` are read in `?q=` and refused here; a file's range is `--`
  ([query.md](query.md#comparisons-on-the-date-keys)).

The design, the prior art and the vector table are
[a date is read where a date is owed](proposals/proposed/2026-08-22-a-date-is-read-where-a-date-is-owed.md).

## The date widget

In the material document `C-c C-s` and `C-c C-d` raise a field rather than a
blind prompt. It stands in the value's own slot — the planning line's, the
line drawn if the row has none — reads the grammar above, and shows what it
will write before it writes it.

| key | what it opens | what `RET` sends |
| --- | --- | --- |
| `C-c C-s` | the row's SCHEDULED value, in the planning line's own slot | `set-planning {keyword: "SCHEDULED", date}` |
| `C-c C-d` | the row's DEADLINE value, the same slot | `set-planning {keyword: "DEADLINE", date}` |
| `RET` on a planning ENTRY | that entry's value — `f` walks into the line and along it, `b` back out | `set-planning {keyword: the entry's own, date}` |

`RET` on the whole planning line is inert and names the way in; the entry under
point is what opens. Over `CLOSED` the box reads that key's own wall: org's
bracket passes through, nothing is offered — there is no vocabulary a reparse
would take — and a phrase is refused in the field with the wall's own sentence,
the box left standing to fix it in.

- **The ghost is the preview.** What was typed stands in the field and the
  resolution rides after it in the mute ink — `10 jan → <2026-01-10 Sat>`, one
  line, the weekday computed. A refusal no further character can rescue shows
  its short word in the marked ink instead; an empty field, a term still being
  written, and a value that IS its own answer show nothing at all.
- **The value opens SELECTED.** A standing stamp comes up whole, so one
  keystroke replaces it and a bare `RET` recommits it byte for byte, the way
  `org-read-date` takes its default.
- **`RET` commits, `ESC` cancels whole, an empty value clears the entry** —
  the foot's own promise, kept. Over a standing offer `RET` takes the offer
  and writes nothing; over the finished value it applies. `S-←`/`S-→` adjust a
  day and `S-↑`/`S-↓` a week in place, the ghost following.
- **What travels is what was TYPED.** The field's resolver draws the ghost and
  writes nothing; the raw text comes to this door, the server resolves it once
  against its own clock, and the pane redraws off that answer. The page spells
  no org, and the two resolvers are pinned against one another over one corpus.
- **One widget, every door.** A drawer pair whose key case-folds to
  `scheduled` or `deadline` routes to the planning line already, and its value
  half wears the same field and the same ghost. `CLOSED` opens the same box on
  its own wall — one widget, two readers, the mode asked in one place so the
  ghost, the offers and the commit cannot disagree. Date-shaped custom
  properties keep the plain box for now.

The same keys over the TABLE are unchanged: they ask over the marked rows
through the shipped prompt, and reach this door with the same grammar.

## `delete`, the one destructive command

Three walls, each checked at this door as well as in the page: the row must
exist, must be **archived first** (`… is not archived: archive it first`),
and the move must land — the blob goes to `trash/` gzipped, a tombstone
where its file was, and the watch delivers the disappearance. Nothing else
in the API removes bytes.

## Examples

```sh
curl -s -X POST -H 'content-type: application/json' localhost:7777/command \
     -d '{"name": "set-state", "ids": ["ROW-ID"], "args": {"keyword": "DONE"}}'
curl -s -X POST -H 'content-type: application/json' localhost:7777/command \
     -d '{"name": "set-planning", "ids": ["ROW-ID"],
          "args": {"keyword": "SCHEDULED", "date": "+1w"}}'
curl -s -X POST -H 'content-type: application/json' localhost:7777/command \
     -d '{"name": "capture", "args": {"text": "TODO Dune", "tag": "book",
                                      "fields": {"Author": "Herbert"}}}'
```
