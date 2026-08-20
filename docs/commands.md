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
| `set-planning` | `{keyword, date}`, null date clears | one `SCHEDULED`/`DEADLINE` entry |
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
must not cross midnight:

- `2026-08-05`, `2026-08-05 09:30`
- `today`, `tomorrow`
- `+3d` and friends — org's own shift charset (`d`, `w`, `m`, `y`)
- org's own `<2026-08-05 Wed>` — kept **verbatim** when it reparses

Anything else is a 400 naming the accepted spellings. Rendered stamps get
their weekday computed for you.

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
