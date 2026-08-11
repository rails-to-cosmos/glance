# Proposal — a capture template is a form description

**Status:** proposed · **Date:** 2026-08-08

Successor to [docs/proposal-capture.done.md](proposal-capture.md), delivered
2026-08-04, which asked WHERE a capture goes and answered it.  This asks
WHAT a capture can say, and answers fixme 11 and 12 together — one
question from two sides.  Element meanings are the org manual's own, off
[Template expansion](https://orgmode.org/manual/Template-expansion.html).
Siblings: `docs/proposal-generalize-ask-kinds.proposed.md` (stage 2 is its fourth
caller) and `docs/proposal-generalize-capture-shapes.proposed.md`.

## What ships today

FOUR CODES, ONE LIST AND ONE SCAN.  `captureCodes`
(`src-query/Glance/Query.hs:2581`) spells `%?`, `%U`, `%T`, `%^{PROMPT}`
with a line of meaning each; `templateParts` (`:2607`) spells the same
four as a case and consults no list; `templatePrompts` (`:2628`) and
`expandTemplate` (`:2638`) are two answers off that one scan.

THE ONE-HEADLINE WALL is `captureText` (`:2563`), which is `oneLine`
(`:2029`): empty-after-strip refused, any newline refused.  It takes the
captured line AND every `fields` answer (`capturedParts`,
`src-web/Glance/Web/Commands.hs:446`).

TWO WRITE PATHS.  Untagged, `captureInbox` (`Commands.hs:348`) appends
`captureEdits` (`Query.hs:2545`) to the tree's `#+GLANCE_CAPTURE_TARGET:`
— `* <text>` plus an `ORG_GLANCE_CREATION_TIME` drawer, no template
involved.  Tagged, `captureBlob` (`:394`) resolves `captureTemplateIn`
(`Query.hs:2698`) — the tag's layer's first heading to EOF, else the
system layer's, else `bareTemplate = "* %?"` (`:2801`) — and composes the
blob through `blobDocument` (`:2755`) at `blobPathIn` (`Blob.hs:59`).

THE FORM is one popup, `openCapture` (`assets/glue.js:2841`): `#ktag`
narrowing over the tree's vocabulary (`drawTagList:2885`), seeded from
the applied query's tag (`filteredTag:2873`); a field per prompt grown
into `#kfields` when the tag settles (`settleTag:2905`); `#ktext` last.
RET walks forward and captures at the line, ESC leaves, a refusal keeps
everything typed.

## Why it reads as almost unusable

1. **A CAPTURE CANNOT CARRY A BODY.**  `captureText` refuses a newline,
   so all a reader contributes is one line, where most org captures are a
   headline plus a paragraph.  `#ktext` is a `<textarea>`
   (`src-web/Glance/Web/Page.hs:136`) and `S-RET` reaches it — `keyName`
   (`glue.js:4687`) names a shifted `Enter` `S-RET`, the listener claims
   `RET` alone — so the widget affords what the wall forbids.
2. **A PLACEHOLDER TITLE STICKS.**  glance keeps the template heading
   verbatim where org-glance's renderer rewrites its title from the
   capture — CLAUDE.md's KNOWN DIVERGENCE.  `book`'s template is
   `* Book\n*** Notes\n    %?` (`test/TestQuery.hs:2630`), so every book
   under it is titled `Book`, the reader's line buried in a grandchild.
3. **EVERY ANSWER IS A BARE ONE-LINE TEXT FIELD.**  `settleTag` grows an
   `<input>` per prompt (`glue.js:2920`) whatever it is for: a date gets
   no control and no validation, a tag no vocabulary though `/capture`
   serves one, a choice no list.  The template can only ask
   `%^{PROMPT}`, so the grammar under the field is the thin one.
4. **THE ANSWERING ORDER IS THE TEMPLATE'S.**  Focus walks tag → prompts
   in `templatePrompts` order → the line, fixed, so the entry's own text
   is answered LAST however early `%?` sits.  Nothing walks BACK — the
   listener claims RET and TAB forward alone.
5. **ESC LOSES EVERYTHING.**  `shutCapture` blanks `#ktag`/`#ktext`/
   `#kfields` and `openCapture` re-seeds, so a capture interrupted to
   look something up is a capture retyped.

## The symmetry

EVERY TEMPLATE ELEMENT IS A TYPED INPUT.  `%^{PROMPT}` is a text input,
`%^{PROMPT|default|c1|c2}` a choice with a default, `%^t` a date, `%^g` a
tag set over a vocabulary, `%^L` a link, `%^{PROP}p` a property pair.
org-capture's escape list IS a widget catalogue whose org-file
serialization is already agreed on.

SO A TEMPLATE IS A FORM DESCRIPTION AND THE CAPTURE FORM IS GENERATED
FROM IT.  One template, read by the scan already there, yields the
fields, their kinds, their domains and their order.  A reader writes the
form by writing the template, in the settings sheet's `#ctpl` box.

THE WIDGETS ARE ALREADY HERE.  Text: the palette's text mode (`askText`,
`glue.js:3182`).  Choice: its field mode over a supplied list (`askFrom`,
`:3209`, the tags popup's `+`).  Date: `planningTimestamp`
(`Query.hs:2439`), behind the line `C-c C-s` raises.  Tags: `/capture`
serves `storeTags`, `drawTagList` narrows it.  Link: `edit-link` and the
link popup's two-field overlay.  Property: the sheet's panel is a
key/value row list.  THE WORK IS A MAPPING FROM CODE TO WIDGET.

## The element table

`never-here` names what this side would have to grow to host the code.

| code | means | widget | verdict |
| --- | --- | --- | --- |
| `%?` | point — where the reader's text lands | the point field, sized by where `%?` sits | ships |
| `%^{PROMPT}` | prompt for a string | text field | ships, gains a kind |
| `%U` / `%T` | capture moment, inactive / active | none — server clock | ships |
| `%t` / `%u` | timestamp, date only, active / inactive | none — server clock | v1 |
| `%%` | a literal `%` | none | v1 (copies through as `%%` today) |
| `%^{P\|def\|c1\|c2}` | prompt with completion and a default | choice field, `askFrom` over the alternatives | v2 |
| `%^t` `%^T` `%^u` `%^U` | prompt for a date | date field, `planningTimestamp` | v2 |
| `%^g` | prompt for tags, target file's vocabulary | tag field over `/capture`'s `tags` | v2 |
| `%^G` | prompt for tags, all agenda files | the same field — one store is one vocabulary | v2 |
| `%^{PROP}p` | prompt for a property value | key/value into the entry's drawer | v3 |
| `%\N`, `%\*N` | the text entered at the Nth prompt | none — a second pass over the answers | v3 |
| `%a` `%A` `%l` `%L` | annotation / link to where capture was called | read-only field over the row at point | v3, decision 2 |
| `%n` | user full name | none — one `system.org` setting | v3 |
| `%<FORMAT>` | `format-time-string` of FORMAT | none | never-here — Emacs's format spec overlaps `Data.Time`'s without equalling it, and a silent divergence in a stamp costs more than a code that copies through |
| `%i` | the region, when capture is called on one | none | never-here — this page has no region; the sheet's textarea selection belongs to another surface |
| `%c` `%x` `%^C` `%^L` | kill ring head, X clipboard, pick one, pick one as a link | none | never-here — the daemon is not the reader's session and holds neither |
| `%k` `%K` | title of / link to the clocked task | none | never-here until a clock exists (fixme 6's territory) |
| `%f` `%F` | file visited when capture was called | none | never-here — a browser visits no file; the nearest fact is the row at point's, which `%a` carries better |
| `%(sexp)` | evaluate elisp | none | never-here — needs an elisp reader |
| `%:keyword` | link-type context | none | never-here — needs `org-store-link`'s plist off Emacs's capture protocol, which has no producer on this side |
| `%[file]` | insert a file's contents | none | never-here — a template naming a path makes the daemon read whatever the request names |

THE SUBSET STAYS ONE LIST AND ONE SCAN.  `captureCodes` grows rows,
`templateParts` grows the matching case arms, `TestQuery`'s zip keeps
them in step, `templatePrompts`/`expandTemplate` stay two answers off one
pass.  The mechanism holds; its size moves.  Everything omitted copies
through, so an Emacs template stays readable here.

## The body problem — fix this first

The wall keeps a capture ONE ENTRY: a blob's first headline is what
org-glance keys it by (CLAUDE.md, Scan), and a newline in the typed line
could land a column-1 star the parser reads as a second entry.  Refusing
the newline is a proxy.  REFUSE THE STAR INSTEAD.

- Split the wall.  `captureText` stays for every `fields` answer — a
  prompt fills a slot inside a line and a newline there is a hole.  A new
  `captureBody` takes the POINT text: refused empty-after-strip, refused
  where any line answers `headingStars` (`Query.hs:2681`), the predicate
  `headingAt` and `topEntry` already ask.  One predicate, three readers;
  `capturedParts` calls one for `agText`, the other per field.
  `blankEntry` is unaffected — the empty arm still makes the first line
  say something.
- `captureEdits` splits at its first newline: the head after the star,
  the drawer, then the rest verbatim in the target's own line endings.
  Still ONE insertion at the end of the file.
- The tagged path needs nothing.  `blobDocument` reads `firstHeadlineOf`
  and measures the drawer off `planningEnd`, so a multi-line expansion is
  what a multi-line TEMPLATE produces; the star refusal keeps
  `firstHeadlineOf` answering the entry the id is minted for.

One predicate, one splitter, one caller, and it moves "almost unusable"
further than anything else here.

## The form

GENERATED, in template order, with the POINT FIELD FIRST.  The tag field
stays where it is, deciding which template is read; then the point field,
the entry being what the reader came to write; then one field per ask in
the order `templateParts` scans them.

- KEYS.  RET moves forward, `S-TAB` and `C-p` move back — the walk the
  listener owes and lacks.  In the POINT field RET inserts a newline and
  `C-c C-c` / `C-x C-s` captures: TWO KEYS COMMIT AN OPEN ELEMENT is
  already the rule for the paragraph textarea and the two-field overlay.
  ESC leaves through `SURFACES`; a refusal keeps the form up as typed.
- WIDGET PER KIND, each a palette mode raised in place: `text` an input,
  `choice` the completing field over its alternatives, `date` a line
  `planningTimestamp` checks before the request goes out, `tags` the tag
  field's narrowing list, `property` a key/value pair, `body` a textarea.
- DEFAULTS are the field's initial value: `%^{P|def|a|b}` opens carrying
  `def`, offered among its alternatives, committed as written untouched.
- BACK-REFERENCES ASK NOTHING.  `%\N` resolves server-side in a second
  pass of `expandTemplate` over the answers it holds, so no field is
  grown for one and no reader answers a question twice.
- The settings box is unchanged: `%` in `#ctpl` raises the code list off
  the server's own `codes` (`glue.js:4148`), so a longer list is offered
  the day it is served.

## What `GET /capture` must add

Additive, one key.  `asks`, beside today's `prompts`:

```
{ "template": true, "prompts": ["Author"], "point": "headline",
  "asks": [ {"name":"Author", "kind":"text", "code":"%^{Author}"},
            {"name":"Shelf", "kind":"choice", "code":"%^{Shelf|a|b}",
             "domain":["a","b"], "default":"a"} ],
  "tags": [...], "codes": [...] }
```

`kind` is one of `text`, `choice`, `date`, `tags`, `property`, `body`.
`domain` rides only where the code names one — a choice's alternatives;
the tag vocabulary stays the top-level `tags` it already is.  `point`
says whether `%?` sits on the template's headline line or below it, which
is how the form knows to draw a line or a box, and it falls out of
`templateParts` with no new scan.

## Staged path

- **v1 — the body and the kinds.**  `captureBody` and the split wall;
  `captureEdits` splitting at the first newline; `%t`/`%u`/`%%` joining
  the list and the scan; `asks` on the wire, every existing prompt kind
  `text`; the point field first, the backward walk, `C-c C-c` in the
  body.  Fixes diagnoses 1, 4 and half of 3.
- **v2 — the asking widgets.**  `%^{P|def|c1|c2}`,
  `%^t`/`%^T`/`%^u`/`%^U`, `%^g`/`%^G`: three scan arms, three `kind`
  values, three existing widgets.
- **v3 — the structural elements.**  `%^{PROP}p` into the drawer
  `blobDocument` already composes, `%\N` as `expandTemplate`'s second
  pass, `%n` off a system setting, `%a`/`%A`/`%l`/`%L` under decision 2.

## Open decisions

1. **Does the template heading's title get rewritten from the capture?**
   org-glance does; glance deliberately does not, and the `book` fixture
   is the cost (diagnosis 2).  RECOMMEND CONVERGING under one rule:
   rewrite the heading's title from the point text ONLY where the
   template's heading LINE spells no `%?`.  A template putting `%?` on
   its headline has said where the title goes; one that does not carries
   a placeholder.  Decidable from `templateParts`.
2. **Does `%a` mean anything here?**  The page HAS a row at point when
   `+` is pressed — `filteredTag` reads the applied query for the same
   reason.  RECOMMEND YES: `%a` expands to
   `[[org-glance-visit:<ID>][<title>]]` of the row at point, empty where
   nothing is selected, drawn read-only so the reader sees what is being
   linked.  `%l`/`%L` are that target undressed, `%A` it with the
   description prompted.  v3, and the one value coming off the CLIENT.
3. **May a multi-line capture go into the INBOX, or blobs only?**
   RECOMMEND BOTH.  A jot with two lines is still a jot, the entry is one
   headline either way, the same `captureBody` decides it, and the inbox
   path pays one split.  Blobs-only would put a rule in the target's
   spelling that has nothing to do with the target.
4. **Should the form save a draft?**  RECOMMEND NO PERSISTED DRAFT AND
   ONE STASH.  A draft outliving the reader's intent is a second store,
   which this repo has a rule against.  What is owed is the sheet's own:
   `stash`/`restore` carry work the reader has NOT committed across a
   remount, and the form becomes one more entry there.  ESC still
   discards, as everywhere else.
5. **Does `asks` replace `prompts` or ride beside it?**  RECOMMEND
   BESIDE, dropping `prompts` once the shell and the harness read `asks`.
   Two producers of one fact for one release costs less than a wire
   change landing in three files at once.
6. **Does the untagged path get a template?**  proposal-capture.md took
   this as "stay bare" and the body change is the reason to keep it so —
   the quick-jot path is one field.  RECOMMEND KEEPING IT BARE: a reader
   wanting a shape names a tag, which is what a tag is for.
