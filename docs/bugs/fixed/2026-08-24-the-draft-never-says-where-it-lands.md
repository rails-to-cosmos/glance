# Bug — the draft never says where it lands

**Status:** fixed · **Reported:** 2026-08-24 (a capture typed into `bicycle`
drew `* [#A]` and no tag) · **Surface:** the capture doc's head row ·
**Fixed in:** `src-web/Glance/Web/Routes.hs`, `test/fixtures/shell-harness.js`

## Symptom

`+`, the tag `bicycle`, `RET`. The sheet opens over the draft and the head row
reads `* [#A]` — the priority the standing filter lent, and nothing else. The
reader is composing into `:bicycle:`, the file line above says
`the capture · :bicycle:`, and the document itself says nothing about it. The
capture then lands under `:bicycle:` all the same, so the pane was the only
part of the flow that did not know.

The same silence covered every **lent** tag: a filter pinning `tag:work`
beside the destination put `work` nowhere the reader could see either, on any
draft whose template spells no title.

## Steps to reproduce

Serve `test/browser/tree`.

1. Filter the table to `priority:[#A]`.
2. Press `+`, type `bicycle`, press `RET`.
3. The head row reads `* [#A]`. `GET /capture?tag=bicycle&priority=A` answers
   `cells.tags: ""`.

## Evidence

- `src-query/Glance/Query.hs:2276` (`draftSeeded`, the `tag` seed) — a lent tag
  is not spliced onto a title-less headline: `| T.null (hrTitle r) = Right []`.
  This parser reads `* :work:` as the **title**, so the org line genuinely
  cannot carry a run before a title stands in front of it, and writing one
  there would be misread on the next load.
- `src-web/Glance/Web/Routes.hs:697` (`draftJSON`) — the draft's cells were
  `cells r`, the very builder a materialized row uses, so `tags` came off
  `hrTags r`: the org line's own run. The **destination** was never in that
  line at all — it joins at the minting (`blobDocument` → `addTagEditsIn`), so
  the draft door answered `""` for it by construction.

## Fix

A display cell is **constructed** and owes no round trip through the org line,
which is what lets it be honest where the line cannot be.

`draftCells` (`Routes.hs:721`) is `cells` with the `tags` entry overridden by
`draftTagsCell` (`:731`): the **destination leads**, the template's own run and
the lent tags follow, deduplicated and folded. `worn` (`:672`) is built the
same way — the destination as the reader settled it, then each lent tag through
the charset `draftSeeded` walls the splice with, so a lent tag org cannot read
is no more worn than it is written. The never-refuse rule is untouched: a
misfit is dropped, never a 400.

The commit carries that cell out as the capture's `tags`, and the minting joins
the destination **idempotently** — `addTagEditsIn` matches folded and answers
`[]` for a tag already in the run — so the blob wears each tag once.

The org line's limit is stated where it is true and nowhere wider: the draft's
own bytes carry no run until a title stands before one, and the commit composes
the header out of the cell rather than out of that line.
