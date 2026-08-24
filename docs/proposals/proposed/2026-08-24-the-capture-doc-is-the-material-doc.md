# The capture doc is the material doc

**Status:** proposed · **Date:** 2026-08-24 · **Origin:** user — capture's
next large redesign: the raw-text form becomes the material doc over a
template-seeded draft.

*The standing flow is [../../capture.md](../../capture.md); its design history
is `done/2026-08-03-capture.md`. This proposal replaces the capture FORM, not
the capture COMMAND.*

## What stands

`+` is a popup form: a tag field, then one field per `%^{PROMPT}` in the tag's
template, then one line of raw text that submits on `RET`. The template
machinery is already server-side and already right — a tag's template is the
first `*` heading of its config layer to EOF, `system.org` the default, the
settings sheet edits the file, expansion never reaches the page
(`docs/capture.md`). What is wrong is the editor: the page's richest surface
is the material doc, and capture — the one moment a reader composes a whole
headline — gets a text box instead.

## The law

**One editor.** The pane already edits a subtree; capture is the pane over a
subtree that does not exist yet. The create pin already exists for exactly
this — an absent file under the empty digest is created
(`docs/invariants.md`, the write path) — so capture is not a second editor
with its own rules, it is the first editor pointed at nothing.

## The flow

1. `+` asks the **tag** first, exactly today's completing field (empty is the
   inbox). The destination question precedes the doc, because the tag picks
   the template, the `#+TODO:` cycle, and where the blob lands.
2. The sheet opens in **capture**: the server expands the tag's template and
   serves a DRAFT — the same shape `/headline` serves, from bytes that exist
   only in the answer. Default template, default draft: `* ` and nothing
   else. The pane draws it as it draws any doc.
3. Every shipped door works on the draft, because the draft is a doc: `RET`
   on the title, the pair box, the date widget and its summon keys, the tags
   door, the state door — offering the TAG'S own cycle, which rides in the
   same config file the template does. Point lands where `%?` stood.
4. `C-c C-c` commits the draft **whole** through today's `capture` command —
   the one door that mints the blob, the shard path, the creation drawer and
   the ledger note. Its args grow from `{text, tag, fields}` to carry the
   draft's cargo (title line, planning, properties, body), the same cargo
   shape the commit door already speaks. `ESC` leaves nothing: no file ever
   existed, so the born-at-open memory is trivially empty.

## Prompts dissolve

`%^{PROMPT}` stops being a pre-form field. The material doc **is** the prompt
surface:

| escape | becomes |
| --- | --- |
| `%^{Author}` in a drawer | the pair, value empty — the pair box edits it |
| `%^{PROMPT}` in the body | an empty slot the pane walks to |
| `%t`, `%u`, stamps | server expansion at draft time, as today |
| `%?` | where point opens |

Prompting escapes existed because the form could not edit structure; the pane
can, so they dissolve into the editors it already has. Stamping escapes stay
server-side — the page spells no org, the draft arrives expanded.

## What this costs

- One read door: the expanded draft for TAG (a small GET beside
  `/properties`; the answer is a headline shape, no file behind it).
- The `capture` command's args widen to the doc cargo; the wire stays one
  command, the walls stay per-key (planning through `plannedValue`, tags and
  keywords through their charsets — the draft commits through the same
  sentences a row edit meets).
- The form's parts 2 and 3 (prompt fields, the line) are deleted from
  `30-capture.js`; part 1 (the tag field) survives as the entry step.

## Refused

- **Multi-headline templates.** One top entry is the law; a template's
  children arrive as the draft's children, but the template contributes one
  headline.
- **Template logic on the page.** The page renders a draft; it never expands.
- **Editing the template from the capture doc.** The settings sheet owns the
  layer file; capture consumes it.
- **A draft that outlives ESC.** No autosave, no draft store; a capture is
  committed or it never was.

## Interactions

- The date widget's summon over a draft planning line is the shipped
  `DraftPlan` machinery — the drafted entry is exactly what capture's
  SCHEDULED slot is before commit.
- The narrowing law holds: the template's headline is the root; typed
  headlines demote below it.
- The cursor lands on the new row when the view carries it, as today.
