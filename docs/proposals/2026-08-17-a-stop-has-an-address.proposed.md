# Proposal — a stop has an address

**Status:** proposed · **Date:** 2026-08-17 · **Origin:** raised directly while
shipping the doc pane's indent guides — should the cursor's position ride the URL
so a view is shareable, and if so what is the thing being named.

## The finding, in one line

A link points AT a material and never INTO one. `refer` makes a relation a link
with a kind (`docs/proposals/2026-08-15-a-relation-is-a-link-with-a-kind.partial.md`),
and the finest thing it can name is the whole material, so "see the harness note
under the picker" is a sentence and not a link.

The address is a thing in its own right; the URL hash is its first consumer.

## What must not be the address

**A byte range.** A row is derived from the body text; `Body.elm` carries `from`
and `to` in bytes and rebuilds them on every parse. An address written as
`position=1234` is wrong after the next keystroke — including a keystroke made in
the pane the link was copied from. A link that rots on the first edit is worse
than no link, because it rots silently and still resolves to SOMETHING.

**A row index.** Same objection, plus it changes when a sibling is added above.

## What is stable

The ancestry chain: the sequence of enclosing stops from the material down to the
one under point. The doc pane already computes it — the indent guides light one
rail per ancestor and the breadcrumb names the same chain, so the address is a
serialization of a thing the pane holds anyway rather than a second walk.

Slugged with org-glance's own rule — downcase, trim, fold whitespace runs to `-`,
applied on WRITE and on READ (invariant 13, already implemented here for kinds) —
the chain survives everything except a rename, and a rename is a fair reason for a
link to need repair.

## The shape

```
/material#of=task/refer-picker&in=harness/drive-mjs
```

- `of` names the material: kind and slug, the pair the tree already indexes by.
- `in` names the stop inside it: the slugged own-line text of each ancestor,
  outermost first, joined by `/`.
- The empty `in` is the material's own headline, which is where the pane opens.

Reading is LENIENT: resolve the longest prefix that still exists and stop there.
An edited document lands the reader near the target rather than nowhere, and the
degradation is monotone — every step of the path that still holds is honoured.

## The unnamed stop, which is the weak part

A paragraph has no title, so it has no slug. Three ways out, none free:

1. **An ordinal within the parent** — `in=harness/3`. Cheap, and wrong the moment
   a sibling is inserted above it.
2. **The first few slugged words of its own text** — stable under insertion,
   dies on a typo fix.
3. **Refuse to address it.** `in` names the nearest NAMED ancestor and the reader
   arrives at the block rather than the line.

(3) is the honest one and is what this proposal recommends for the first cut: an
address that is wrong is worse than an address that is coarse. (1) can be layered
on later as a disambiguator (`in=harness/3`) once there is a reason to want it.

## Three traps that decide the implementation

- **`replaceState`, never `pushState`.** Every `n`/`p` would otherwise stack a
  history entry, and Back becomes a cursor rewind.
- **A hash-only change is a same-document navigation.** Nothing reloads, so the
  page must listen for `hashchange` rather than expect a boot. The refer spike's
  runners carry a workaround for exactly this
  (`docs/spikes/2026-08-16-refer-picker/README.md`).
- **The address is written from the pane and read by the boot**, so the two must
  agree on the slug rule or a link will fail to resolve the document that wrote
  it. One function, called from both ends, tested against org-glance's own
  `org-glance--kind-slug`.

## Sequencing

After the indent guides land, because the guides are what put the ancestry chain
in the pane. Landing them first gives the address ONE source of truth.

Then, in order: the chain serializer with its own tests; the hash written on
every move; the boot resolving it leniently; and only then `refer` learning to
carry an `in=` so a relation can point at a line.
