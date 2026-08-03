# Proposal — capture as org-glance does it

**Status:** draft · **Date:** 2026-08-03

Replace the flat inbox capture with org-glance's own flow: ask for a tag,
expand that tag's capture template (or the default), create the entry **in
the store** — a real blob with a minted id that Emacs adopts through the
`EXTERNAL.jsonl` contract already in place.

## The flow (`+`)

1. **Tag prompt** — completion over `storeTags`, matching optional: free
   text names a tag that does not exist yet (the charset wall refuses
   garbage server-side, as `manage-tags` does).  ESC cancels.  An EMPTY
   answer falls back to today's inbox capture — the untagged path stays.
2. **Template** — the tag's own capture template when its config declares
   one, else the default template, else the bare `* %?` equivalent.
3. **Fill** — the template's prompts resolve in the palette (`%?` is the
   text the reader types; see the expansion subset below).
4. **Write** — a new blob in the canonical store, org-glance's own layout:
   mint `ORG_GLANCE_ID` (org-glance's `graph:make-id` is `org-id-uuid` +
   the sharded path — `data/<2>/<rest>/data.org`), stamp
   `ORG_GLANCE_CREATION_TIME`, wear the tag, append the id to
   `meta/EXTERNAL.jsonl`.  Emacs's `graph:refresh-external` adopts it —
   the WAL gains the record without Emacs having captured anything.
5. The watch delivers the row (the store walks blobs); the cursor lands on
   it (the landing rules' newest citizen — an explicit create may land its
   row, worth deciding beside `apply`/`pop`/`archive`).

## Template storage — the config files already own it

org-glance's convention (verified in its source and this corpus): a tag's
capture template is the first `*` heading of `config/tags/<tag>.org` — the
same file that carries the tag's `#+TODO:` cycle.  The default template
lives in `system.org` the same way.  No new file class, no new pragma:
**the layer file is the template file**, and `GET/POST /config` already
serves and splices these files verbatim, drift-locked.

## Settings surface

The settings sheet's layer machinery extends: beside the cycle box, the
selected layer shows its **capture template** (the first heading, verbatim
— the sheet has no org parser and does not need one: the server slices the
heading's extent and splices it back like a pragma line).  Default =
system layer's; per-tag = that layer's.  Same sync semantics, one write
per file.

## Expansion subset

org-capture's full language is enormous; the corpus uses a sliver.  v1:

- `%?` — point: the palette's typed text lands here.  Required.
- `%U` / `%T` — inactive / active timestamp, server clock.
- `%^{PROMPT}` — a named ask; v1 renders each as one palette field in
  order (the multi-field ask is the edit overlay's shape).
- Everything else copies through verbatim (a template using an unknown
  `%`-code captures it literally — honest, visible, refusable later).

Typing `%` in the settings template box completes over exactly this
subset — each code with its one-line meaning.  The completion IS the
contract's window: what it offers is what expands, what it omits copies
through as written.  (The subset list lives once, server-side; the box's
completion reads it off the served page rather than spelling it again.)

## Server shape

`POST /command capture {tag?, text, fields?}` — the command grows the tag
and the field answers; the server expands the template (expansion is
server-side: the client never holds template logic), mints, writes,
notes.  Refusals: charset tag, template a reader's answer cannot fill,
store-root missing.  The old `#+GLANCE_CAPTURE_TARGET` inbox path remains
the untagged fallback and keeps its config line.

## What it buys

Capture from any device the daemon reaches lands as a first-class
org-glance headline — id, creation time, tag, template shape — and Emacs
sees it on its next refresh without importing anything.  The phone half of
the capture exit-bar stops being blocked on a UI question about `+`
(the tag prompt is a palette; palettes already have the touch-gap noted).

## Open decisions

1. Landing rule for an explicit create (select the new row? the archive
   anchor's sibling question).
2. `%^{PROMPT}` v1: one palette per prompt in sequence vs one multi-field
   overlay.  Sequence is simpler; overlay is one interaction.
3. Whether the untagged inbox path should ALSO template (system default)
   or stay bare `* text` — bare keeps the quick-jot path frictionless.
