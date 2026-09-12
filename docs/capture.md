# Capture

`+` captures a headline from any device the daemon reaches, org-glance's own
way: a tagged capture is a real blob in the store — id, shard path, creation
stamp, ledger note — that Emacs adopts without importing anything. The
README's Capture section is the crib; this page is the whole law. The design
history is `docs/proposals/done/2026-08-03-capture.md`; the form and its sheet
were retired by
`docs/proposals/proposed/2026-09-12-capture-is-a-row-in-the-table.md`.

## The flow

**A capture is a row.** `+` splices a **draft row** into the table already on
screen, below the row at point, seeded from the standing filter, with the title
cell's editor open. There is no form and no sheet: the table can already say
everything a capture says.

| key | over the draft |
| --- | --- |
| `TAB` / `S-TAB` | walk the cells — title, state, priority, tags — wrapping |
| `RET` | commit, from any cell |
| `ESC` | drop the draft whole |

Three laws hold it:

1. **A draft always carries an open editor.** The open input takes every key it
   sees, so `n` in a draft types an `n`. An editor-less draft would be a row
   with no id, no span and no file that the movement keys could stand on.
2. **No other row can reach a draft.** It is never marked, never selected —
   by key, by click or by long press — never sorted with the rest, never
   filtered away, and its reserved id never reaches a `/command`.
3. **`ESC` leaves the rows byte-identical.** No file was written, so nothing is
   put back — the draft is spliced out and the count is the count it was.

`+` under a live draft puts the editor back on the title rather than splicing a
second one — and leaves it alone where it already stands there. A `/headlines`
answer arriving under a draft — a WAL tick, a poll, a filter change — leaves it
standing with what the reader had typed, and the caret where it was.

**`RET` is the whole capture at one press.** The open cell's value is folded in,
the draft is spliced out, and the fresh rows are asked for at once; point
follows the id the command answered onto the **first settle that carries the
row**, wherever `sort:` puts it. A capture has no SCHEDULED, so the undated tail
is where it belongs whatever row it was typed beside. The wait is bounded: ten
settles without the row, or a query change, drop the id rather than let it
spend a later write's settle.

**A refusal keeps the draft standing** — a row that cannot commit is a row the
reader would otherwise have to retype. `RET` on an empty title says `nothing to
capture`, the server's 400 says its own sentence, the word **leads** the row's
hint — `nothing to capture · → inbox`, so the destination never disappears
behind it — the editor comes back to the title with its text selected, and the
dress turns warn. The next content keystroke clears both; a walk and a movement
leave them; only `ESC` dismisses.

**A row carries no body, no drawer and no planning line**, so the commit's args
are the title, the destination as `tag`, the row's own run as `tags`, and the
two scalars. A template seeds nothing — the server already ignores it on this
road.

## What the filter lends

The filter's **positive, pinned** atoms fill the draft. A negation, a `+`
widening, an alternation and a `*meta*` each describe a *set* of rows rather
than a value a capture could wear; a scalar named twice describes a union and
lends nothing.

| the filter pins | the draft wears |
| --- | --- |
| the first `tag:` | the destination: the blob's layer |
| every later `tag:` | the draft's own tag cell |
| one `state:` | that keyword, if the destination's cycle has it |
| one `priority:` | that letter, `[#B]` folded to `B` |
| anything else | nothing |

**The destination is said in the row** as `→ book` or `→ inbox`, in the row's
own `hint`, which the renderer draws in the last column the row carries no cell
for — free space at the right-hand end, the DEADLINE cell in this table. It is
a row field rather than a cell value, and never a date. The title cell cannot
hold it: the cell editor empties the cell it opens in, so a hint drawn beside
the title is wiped the moment the editor arrives.

**A state the destination's cycle lacks is dropped, and the hint says so.** `+`
asks `GET /keywords?tag=NAME` for the cycle at the same moment it draws the
row, and only where the filter seeded a state — with none there is nothing to
check. The answer either confirms the seeded state or clears it, and the hint
reads `→ book · NEXT dropped`. The wire never carries the refused state, so the
commit's own wall stays exactly as strict as it is for every other caller,
the MCP `capture` tool included.

**Seeding never refuses a capture.** A fact the commit's walls would turn down
is simply never seeded. It is the filter talking about other rows.

**The draft's keys belong to the editor.** An open cell stops every key it
sees, so the shell's dispatch never hears one and `Keymap.hs` gains no row and
no scope. The seam is one table-view mount option, `onCellKey(e, {id, col,
key, value})`, asked at the head of the cell editor's keydown.

**The row is the renderer's.** The draft is a `producer` row: it carries
`under` (the row it stands beneath), `hint` and `refused`, and the widget
places it, dresses it and paints it through every sort, filter, page and
socket delta. A `/headlines` answer replaces the store's rows and leaves it
standing, the open editor and its caret with it.

## Templates

A tag's capture template is the **first `*` heading of its config layer to
the end of the file**, verbatim — the same `.org-glance/config/tags/TAG.org`
that carries the tag's `#+TODO:` cycle; everything above the first heading
is the pragma region. The default template lives in `system.org` the same
way. The layer file *is* the template file: the settings sheet shows and
edits it beside the cycle box, one drift-locked write per file.

```org
# .org-glance/config/tags/book.org
#+TODO: TODO READING | READ
* Book
:PROPERTIES:
:AUTHOR: %^{Author}
:END:
%?
```

### The expansion subset

Expansion is server-side; the page never holds template logic.

| code | expands to |
| --- | --- |
| `%?` | nothing — it is **where point opens** |
| `%U` | inactive timestamp, the server's clock |
| `%T` | active timestamp, the server's clock |
| `%^{PROMPT}` | its **empty value**: a drawer pair with none, a slot in the body |

The expansion codes serve the **older road** alone — `text` with `fields`,
which org-glance drives. A draft row carries no template: it is composed out of
its cells, and `title` names the road that ignores the template entirely. The
stamping escapes stay server-side: the page spells no org.

Anything else copies through **verbatim** — an unknown `%`-code stays
visible in the captured entry and the capture still lands. Typing `%` in
the settings template box completes over exactly this subset; what the
completion offers is what expands. That list is **closed**: `RET` there
takes an entry off it, and a code the expansion does not know is typed into
the box by hand rather than through the completion.

A template with no `%?` has nowhere for the older road's text to go, and is
refused at the commit.

## The two doors

### `GET /keywords?tag=NAME` — the destination's cycle

One member, and nothing else:

```json
{"states": ["TODO", "DONE", "READING", "READ"]}
```

`/keywords` is otherwise **row-keyed**, and a draft has no row; `?tag=` is the
same question asked for a row that does not exist yet. The answer is the FLAT
list — every word of every scope, widest first — which is what the page checks
a seeded keyword against and the very list the commit door walls with. An empty
`?tag=` is the inbox, whose scope is the tree's own.

It is composed off the destination alone: the template is never read, so a
layer whose template has no `%?` is still a 200 here and meets its refusal
where bytes are written. **No file is created**, and there is nothing to
inherit into — the draft is a row the page types, so the door takes no
`?state=`, no `?priority=`, no `?tags=` and no day.

### `POST /command {"name": "capture"}` — the commit

**Two roads, exactly one taken.** `text` (with `fields`, through the tag's
template) is the older wire and stays — the door is public, and org-glance may
drive it. `title` opens the cargo road, which the draft row posts:

```
{tag?, title, state?, priority?, tags[], planning[[K,V]], properties[[K,V]], body}
```

Naming both `text` and `title` is **refused** rather than resolved. Both roads
hand the same org to the same minting, which is why the shard path, the id,
the creation drawer, the ledger note and the inbox split are untouched by the
widening.

`title` is title text alone — no stars, no state, no tag run. The header is
composed and then **read back**: a title that reparses as something else is
refused naming the part, rather than written and misread on the next load.

**One wall per key, and every one of them the wall a row edit meets:**

| key | wall |
| --- | --- |
| `title` | one line, then the headline reparse |
| `state` | the draft's own cycle — the list `GET /keywords?tag=` offered |
| `priority` | org's single letter |
| `tags` | the org tag charset |
| `planning` | `plannedValue`, **the key outranking the value** |
| `properties` | none, exactly as the commit door's drawer list has none |
| `body` | one top entry: a body line opening a single star is refused |

## What a tagged capture writes

org-glance's own layout, minted the way `graph:make-id` does:

- a fresh `ORG_GLANCE_ID` (a bare `org-id-uuid`, no tag prefix),
- the blob at `data/<2-char shard>/<rest>/data.org` under the store,
- `ORG_GLANCE_CREATION_TIME` stamped, the tag worn on the headline,
- one line appended to `meta/EXTERNAL.jsonl` — the contract by which Emacs
  learns of writes it did not make.

The watch then delivers the row (the store walks blobs; the write nudges
its own fresh directory into the watch queue, since fsnotify never looks
into a directory it has just armed).

An **untagged** capture appends to the inbox and mints nothing: no id, no
tag, no ledger line. It takes the blob path's drawer splice and nothing else,
so the creation stamp joins whatever drawer the entry already carries.

## What Emacs sees

`org-glance-graph:refresh-external` replays the ledger. A caveat worth
knowing, tracked in `AGENTS.hs` and pinned red-if-fixed in the interop
suite: org-glance currently **skips a ledger id it has never seen**, so a
glance-minted capture waits on the org-glance side of that contract (the
adopt-the-unknown-id fix); an id org-glance already knows re-derives
cleanly, tags included. See
`../bugs/open/2026-08-20-a-tag-added-by-glance-is-invisible-to-org-glance.md`
for the same seam from the tag side.

## Refusals

Coarsest first, every one of them ahead of a byte: a missing store root; a
capture that is not one headline; a template with no `%?`; an unanswered
prompt on the older road; a template that expands to no headline. All of them
are spoken at the **commit** door, the read door answering a cycle no template
can spoil. Each is a spoken refusal — nothing is written.

## Refused designs

- **Multi-headline templates.** One top entry is the law; a template's
  children arrive as the draft's children, but the template contributes one
  headline.
- **Template logic on the page.** The page renders a draft; it never expands.
- **Editing the template from a capture.** The settings sheet owns the layer
  file; the older road consumes it.
- **A draft that outlives `ESC`.** No autosave, no draft store; a capture is
  committed or it never was.
- **A tag field.** The destination is read off the filter and stated in the
  row. The tags cell is one `TAB` away.
- **A per-cell write before the commit.** The draft's cells accumulate; the
  whole capture goes out at one `RET`. A draft has no id and no span for a
  per-cell verb to name.
- **A filter-lent planning day.** A day is no fact the filter pins into a
  capture, and the destination hint is a row field rather than a date cell.
