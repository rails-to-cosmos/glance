# Capture

`+` captures a headline from any device the daemon reaches, org-glance's own
way: a tagged capture is a real blob in the store — id, shard path, creation
stamp, ledger note — that Emacs adopts without importing anything. The
README's Capture section is the crib; this page is the whole law. The design
history is `docs/proposals/done/2026-08-03-capture.md`.

## The form

`+` is one form, three parts, `RET` moving forward:

1. **Tag** — a field completing over the tree's own tag vocabulary,
   commonest first. Free text names a tag that does not exist yet (the
   server's charset wall refuses garbage, the same wall `manage-tags` has).
   An **empty tag is the inbox**: the capture lands in `<root>/inbox.org`
   as bare `* text` with its creation drawer — the quick-jot path, byte for
   byte the old shape.
2. **The template's own fields** — grown in place once the tag settles, one
   field per `%^{PROMPT}` in template order.
3. **The line** — the text that lands at `%?`. `RET` captures; `ESC` leaves
   the form whole.

The cursor lands on the new row when the current view carries it, and stays
put when the view filters it out.

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
| `%?` | the typed line — required, the point of the capture |
| `%U` | inactive timestamp, the server's clock |
| `%T` | active timestamp, the server's clock |
| `%^{PROMPT}` | one form field, asked in template order |

Anything else copies through **verbatim** — an unknown `%`-code stays
visible in the captured entry and the capture still lands. Typing `%` in
the settings template box completes over exactly this subset; what the
completion offers is what expands.

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

A tag outside the charset wall; a template with a prompt the answers do not
fill; a missing store root. Each is a spoken refusal — nothing is written.
