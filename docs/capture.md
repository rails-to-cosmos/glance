# Capture

`+` captures a headline from any device the daemon reaches, org-glance's own
way: a tagged capture is a real blob in the store — id, shard path, creation
stamp, ledger note — that Emacs adopts without importing anything. The
README's Capture section is the crib; this page is the whole law. The design
history is `docs/proposals/done/2026-08-03-capture.md`, and the redesign that
made the capture doc the material doc is
`docs/proposals/proposed/2026-08-24-the-capture-doc-is-the-material-doc.md`.
What that redesign got wrong on its first day, and why each rule below reads
the way it does, is four files under `docs/bugs/fixed/`:
[the draft never says where it lands](bugs/fixed/2026-08-24-the-draft-never-says-where-it-lands.md),
[a rich draft opens with no editor](bugs/fixed/2026-08-24-a-rich-draft-opens-with-no-editor.md),
[an empty title's edit swallows its own line](bugs/fixed/2026-08-24-an-empty-titles-edit-swallows-its-own-line.md),
[a planning phrase stays raw in the draft](bugs/fixed/2026-08-24-a-planning-phrase-stays-raw-in-the-draft.md).

## The flow

**One editor.** The sheet already edits a subtree; capture is the sheet over a
subtree that does not exist yet. Two steps:

1. **Tag** — a field completing over the tree's own tag vocabulary,
   commonest first. Free text names a tag that does not exist yet (the
   server's charset wall refuses garbage, the same wall `manage-tags` has).
   An **empty tag is the inbox**: the capture lands in `<root>/inbox.org` —
   the quick-jot path. The field opens on the tag the standing filter names,
   so a filtered table needs no keystroke here.
2. **The doc** — `GET /capture?tag=TAG` expands the tag's template and answers
   a **draft**, and the sheet opens over it in capture mode. The draft is a
   doc, so every shipped door works on it: `RET` on the title, the pair box,
   the date widget and its summon keys, the tags door, the state door —
   offering the tag's own cycle. Point lands where `%?` stood.

**Every draft opens editing.** A reader who asked for a capture is composing
one, so the pane opens the editor at the place `%?` named rather than asking
for a `RET` first. Point on the headline is the **title edit**; a `%?` on a
body line is the **paragraph editor** over that line, seeded with what the
template left standing there and the caret at its end (`point` names a line,
not an offset). A line no editor claims — a template's own child headline —
keeps point, and the pane's `RET` is the way in as on any doc.

`C-c C-c` commits the draft whole. `ESC` leaves nothing: **no file ever
existed**, so there is nothing to undo — no autosave, no draft store.

**The bare-draft law.** Where the draft is the bare default — star, space,
empty title and nothing else — the title edit *is* the capture: `RET` on the
typed title commits immediately, so the inbox jot stays `+`, `RET`, the line,
`RET`, key for key. The **destination tag does not make a draft rich**: it is
the address `+` already asked for, so a bare template under a tag is the bare
draft too. A template with more than a bare headline commits on `C-c C-c`
alone, and `RET` on the title just closes the title edit as usual.
`ESC` in the bare title edit drops the capture whole.

On a rich draft the box the landing opened is an ordinary sheet edit, so the
**standing ladder** holds: `RET` closes it and `C-c C-c` behind it takes the
capture; `ESC` closes it and the next `ESC` drops the draft.

**The planning line resolves in the draft.** A row's planning phrase is posted
and comes back transformed; a draft posts nothing, so it resolves the phrase
itself with the **ghost's own reader** — the pane says what the file will
hold. What travels at `C-c C-c` is still the **raw phrase**: the wall
transforms it once, against the server's clock. A phrase the resolver refuses
stays as it stands and meets the wall's own sentence.

The cursor lands on the new row when the current view carries it, and stays
put when the view filters it out.

## What the filter lends

A capture inherits from the **current filter** whatever the template leaves
unspecified. **The template speaks first**; where it is silent, a fact the
standing filter pins to *one ordinary positive value* fills the gap:

| the filter pins | the draft gets |
| --- | --- |
| `tag:book` | the destination — and with it the template and the cycle |
| `state:TODO` | the keyword, where the template spells none **and** the draft's own cycle declares it |
| `priority:[#B]` | the priority, where the template spells none |
| further `tag:` terms | joined to the draft's run, beyond the template's |
| `scheduled:2026-09-09`, `deadline:friday` | that entry, where the line has none |

Never a negation, never an alternation, never a `*meta*`. The page extracts
the facts from the parsed query and passes them to `GET /capture` as optional
arguments; the **server** merges them template-first, so one composer owns
precedence and a day word resolves under the door's one clock read.

**Inheritance never refuses a capture.** A lent fact the draft's own walls
turn down — a state outside its cycle, a letter that is no priority, a tag
outside the charset, a day the grammar will not read — is simply not
inherited. It is the filter talking about other rows.

**The draft says where it lands.** Its tag cell is *constructed* — the
destination first, then the template's own run and the lent tags — so the head
row wears the destination before a title is typed. The commit carries that
cell out as the capture's `tags` and the minting joins the destination
idempotently, so the blob wears each tag once.

The **org line** is the one thing that cannot spell it: the parser reads
`* :work:` as the title itself, so the draft's own bytes carry no run until a
title stands in front of one. The display cell says it all the same, and the
commit composes the header out of the cell rather than out of that line.

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

The prompting escape dissolved. `%^{PROMPT}` was a pre-form field only
because the form could not edit structure; the pane can, so the ask arrives as
the editor it belongs in — the pair box for a drawer, the body walk for a
slot. The stamping escapes stay server-side: the page spells no org.

Anything else copies through **verbatim** — an unknown `%`-code stays
visible in the captured entry and the capture still lands. Typing `%` in
the settings template box completes over exactly this subset; what the
completion offers is what expands. That list is **closed**: `RET` there
takes an entry off it, and a code the expansion does not know is typed into
the box by hand rather than through the completion.

A template with no `%?` has nowhere for point to open, and is refused **when
`+` opens** rather than after a whole entry has been composed over it.

## The two doors

### `GET /capture[?tag=NAME]` — the draft

The shape `GET /headline` serves, field for field, off bytes that exist only
in the answer. **No file is created.** `id` is null, `file` is empty and
`digest` is `""` — the **create pin**, the very lock the write path already
spells for a target that is not there, which is what makes the commit that
follows an ordinary drift-locked write.

Three members ride beside the headline shape:

- **`point`** — an integer line of `body`, or `null` for the headline row.
  The coordinates `ownLines` and a child's `line` are already in, so the pane
  lands by a reading it makes anyway. Line 0 *is* the headline, so the
  integer form never names it; a `%?` standing in the planning line or the
  drawer answers `null` too, those being lifted out of `body`.
- **`cycle`** — the tag's own TODO words in the shape `GET /keywords` answers
  in, one entry per source, widest first. It rides here because `/keywords`
  is **row-keyed** and a draft has no row. The cycle the state door offers is
  the list the commit door walls with.
- **`tags`** — the tree's whole tag vocabulary, for step 1's field. It rides
  here rather than on `/tags` because a capture names no rows.

The optional inheritance arguments are `?state=`, `?priority=`, `?tags=a,b`,
`?scheduled=` and `?deadline=`.

### `POST /command {"name": "capture"}` — the commit

**Two roads, exactly one taken.** `text` (with `fields`, through the tag's
template) is the older wire and stays — the door is public, and org-glance may
drive it. `title` opens the sheet's own cargo:

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
| `state` | the draft's own cycle — the list `GET /capture` offered |
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
prompt on the older road; a template that expands to no headline. The two a
template can be wrong in are raised at the **draft** door as well, so a broken
layer is named when `+` opens. Each is a spoken refusal — nothing is written.

## Refused designs

- **Multi-headline templates.** One top entry is the law; a template's
  children arrive as the draft's children, but the template contributes one
  headline.
- **Template logic on the page.** The page renders a draft; it never expands.
- **Editing the template from the capture doc.** The settings sheet owns the
  layer file; capture consumes it.
- **A draft that outlives `ESC`.** No autosave, no draft store; a capture is
  committed or it never was.
