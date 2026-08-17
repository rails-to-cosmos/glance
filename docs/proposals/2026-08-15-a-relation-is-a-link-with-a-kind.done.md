# Proposal — a relation is a link with a kind, so the link ships first

**Status:** done — LAYER 1 DELIVERED 2026-08-16 (`@` links a headline into the
prose), LAYER 2 DELIVERED 2026-08-17 (`K`, or `kind:` in the picker's own
filter, declares the kind; the slug is the server's) · **Date:** 2026-08-15 · **Origin:** user, framing stage 5b of the
relations proposal as a layering: *popups → org-links in the material sheet →
relations on top, because relations are links.*

This takes no model decisions. The protocol, `?kind=`, id-only resolution and
the reverse index are settled in `2026-08-12-relations.partial.md`; every one of
them is a dependency here, never a question.

## The layering, and where it got to

| layer | what it is                                     | state                                |
|-------|------------------------------------------------|--------------------------------------|
| **0** | the popup machinery                            | already existed; nothing built       |
| **1** | `@` inserts a **plain org link** to a headline | **shipped 2026-08-16**               |
| **2** | `K` in the picker asks a **kind**               | **shipped 2026-08-17**               |

Layer 1 was shippable alone and is: 42% of rows carry no `ORG_GLANCE_ID` and
cannot be linked to, and the other 58% are exactly what a reader wants to point
at from a sentence.

## The chord that could not be pressed

Both rehearsals opened the kind stage with a PREFIXED `@` — `C-u @`, org's own
universal argument. Two findings killed that spelling, 2026-08-17.

**`C-u` is the browser's.** It is view-source, and glance's dispatch calls
`preventDefault` on any press that merely OPENS a sequence
(`frontend/glue/70-shell.js:224-226`), so binding `C-u @` would swallow every
`C-u` in the app — the reader loses view-source at the prefix, before the second
key decides anything. `C-u` is now a reserved chord (`AGENTS.hs`'s `reserved`).

**`C-c @` cannot be pressed where it is most wanted.** The same dispatch
declines to claim a prefix while a selection is live — *"a live selection makes
C-c and C-x copy and cut"* (`70-shell.js:88`, `selecting()` at `:89-93`). So
over a selected region `C-c` copies, the pending list stays empty, and the
following `@` matches the PLAIN binding and opens the kind-less picker. Layer
1's headline feature — **a selected region becomes the link** — and a
`C-c`-prefixed kind are mutually unreachable, and the reader is told nothing.

**So the kind is a rung of the picker rather than a chord that opens it.** `@`
raises the picker exactly as it does today; `k` while it is up asks for the
kind; `RET` links. Nothing is prefixed, so a region behaves identically, and the
kind stays optional in the way the proposal always wanted — a plain mention is
still `@` then `RET`.

It also settles an ordering the two proposals disagreed on. The relations
proposal has the headline first and *"the kind, optional"* second
([`2026-08-12`](2026-08-12-relations.partial.md), "the verb"); this one had
*"kind first, then the row"*. **Row first**, which is what the shipped picker
already opens on, and what "related how?" reads as after a row is in hand.

## What shipped

### The verb

`@` over the materialize sheet raises a picker, RET links the row under the
cursor into the prose. `src-web/Glance/Web/Keymap.hs:114` binds it in the
`modal` scope — **the same key the table already carries in `table` scope**,
which is org-glance's own split: the overview drills into the rows referring to
this one, the sheet links one INTO the prose. `live` gates by scope so the two
never fire together.

`@` is a character first. It opens the picker at a **word boundary** alone —
the line's start or after whitespace — so `dmitry@example.com` typed into prose
stays an address. The binding claims the key, so the literal is written by the
handler rather than by the browser.

**A selected region becomes the link**, and its own words are what the link
reads as: select `weekly`, take `MDE weekly`, and the prose reads
`[[glance:…][weekly]]`. A region is as explicit as a prefix, so no boundary
gates it.

### One pipeline, two doors

`GET /refer` (`Routes.hs:190`) is **not** a second endpoint in any sense that
matters. `headlines` and `refer` are both one line over `viewPage`
(`Routes.hs:210`), which narrows, hides the archive, orders, cuts to the page
and encodes. A door may add exactly one thing — a `keep` predicate — so the
picker mounts the very view the table mounts and no second encoder exists to
drift.

Two cuts are the picker's, and both are cuts rather than refusals a reader meets
after choosing:

- a row with no `ORG_GLANCE_ID` cannot be linked to;
- a row is not its own reference (`?row=ID`).

### The picker is a table-view mount

`frontend/glue/60-refer.js` mounts the renderer in its own `inline` mode: chips
alone until `/` summons the editor onto their line, no title, no hint line, no
sort marks, a capped window.  The columns, the badge hues, the cursor, the filter
grammar, its suggestions and `DEL` all arrive built.  How it got there — a
hand-rolled table, a spike, and the fold back into the renderer — is under
"Where this proposal was wrong".

What is NOT duplicated is the narrowing: `GET /refer` is the server's own
pipeline and every query change re-asks it. The suggestions are the store's too: `/refer` answers a `tag:` vocabulary
over every row the query matched, and the `state:`/`priority:` domains ride
their columns as they do for the table.

- It opens on the tree's own `views.default`, so the rows it offers are the rows
  the table would be showing, and `columns:state,priority,title,tag` rides the
  query rather than a second view.
- `marks`, `flags` and `actions` do not arise: one row is being chosen, so there
  is nothing to mark, nothing pending, and picking is not modifying.

### One box takes the write

With a paragraph open the link lands at its caret. With none open,
`insertHere` — **`+`'s own path** — draws the draft row and opens one. Both
cases end in `#dtext`, so the sheet's existing drift-locked
`POST /headline {body, properties, planning, digest}` is the only write route.
No new command, no new write path, no twelfth handler.

### The protocol

`glance:` was already in `refPrefixes` (`Query.hs:540`), so a link written today
resolves today and `ref:` follows it. Layer 1 needed no protocol change.

## How the design was chosen

Three rehearsals were built and driven in real Firefox and Chromium before a
line of shell code was written —
[`../spikes/2026-08-16-refer-picker/`](../spikes/2026-08-16-refer-picker/),
open `index.html`:

| | | |
|---|---|---|
| **A** | a modal `.pop-band` over the veil | rejected: the pane it writes into is covered |
| **B** | an inline completion under the caret | rejected: the query lives in the prose, which fights org text |
| **C** | **the table-view itself, shrunk** | **chosen** |

C won because it is the surface the reader already reads: same schema, same
hues, same `/`, same cursor, and — the decisive part — it is a real table-view
mount, so it inherits the filter grammar instead of growing a second one.

Decisions taken against the rehearsals, each now in the shipped code:

- **the top row is selected**, so RET takes it;
- **the kind is a rung of the picker, not a chord that opens it** — settled
  2026-08-17, against both rehearsals. See "The chord that could not be
  pressed" below;
- **`@` only at a word boundary**;
- **a selected region becomes the link**;
- **`DEL` walks the query down** — and the shipped ladder SWAPS a rung rather
  than growing one: the kind stage never shipped, and the summoned filter box
  became a rung of its own. What is typed, then the box itself, then the chips,
  then the `@`. The first two are the mount's, taken while it holds the keys;
  the last two are the picker's.
- **the region is the link's words and never a filter**: seeding the filter with
  the reader's prose narrows the store by an accident of phrasing and puts prose
  on the chip strip;
- **the cursor is a ground and nothing else** — a badge keeps its own hue on the
  cursor row;
- **a badge column's header aligns with its badges' first letter**, which landed
  in the real renderer as well;
- **`ESC` while filtering is one step** — the half-typed filter is dropped AND
  the cursor lands on a row, because a compact table is a thing to pick from and
  an emptied box is an editor the reader was already done with;
- **the link goes into whichever box is open** — a title edit takes it into the
  TITLE, rather than drafting a body line underneath to hold it.

## Where this proposal was wrong

Stated plainly, because the original text is still above:

- **`GET /refer` was to compose `insert` server-side.** It does not. The view
  JSON already carries the id and the title, and composing the bracket grammar
  in the page costs one template string; a second server field would have been a
  second place the link spelling is written. The page spells `glance:` and
  nothing else.
- **The picker was to be `.pop-band`, the sheet's popup tier.** It hangs at the
  caret instead, with no veil, and takes a level of its own (`ZRefer`, 102 —
  `AGENTS.hs:3369`) because it is drawn INTO the sheet and a level under it puts
  the picker behind the prose it is completing.
- **The picker was to be a table-view mount.** It is one — but only after the
  detour. The port shipped the spike's hand-rolled table first, on the reading
  that a mount could not be a compact caret-anchored box; that reading was wrong
  about the renderer rather than about the widget. What the compact box needed
  was a MODE, and the mode is now the renderer's: `inline: true` — chips without
  a title, the editor summoned rather than resident, no hint line, no sort marks,
  a capped window. The second table is gone, and the columns, badge hues, cursor,
  filter grammar, suggestions and `DEL` are the renderer's single copy.
- **`@` was to be written into the pane and then replaced.** Nothing is written
  until a row is taken, so ESC and DEL cost the reader nothing at all — which
  also means `DEL`'s last rung is simply to close, where the rehearsal deleted
  an `@` it had inserted.
- **`refs` — the in-degree — is not shown.** It needs the reverse index
  (relations stage 4). An absent column beats a faked one.

## What is still owed

- ~~**The kind stage.**~~ Shipped 2026-08-17. `Ref` carries the kind
  (`src-query/Glance/Query.hs`), `GET /refer` answers `kinds` counted in rows and
  echoes the canonical slug of whatever `&kind=` was asked, and the picker takes
  it two ways — `K`, or `kind:` typed into its own filter, which is the chip that
  removes it again. What is still owed is **stage 4 of the relations proposal**,
  the reverse index: a kind can be written and read, and nothing yet asks *what
  points at this row, and how*.
- **`refs` on the row**, after relations stage 4.

## What catches it going wrong

| | |
|---|---|
| `test/TestServe.hs:8482` | `GET /refer` — four cases: the shape does not fork from `/headlines`, both cuts bite, the query narrows alike |
| `test/browser/cases.mjs:514` | `@` raises at the caret, RET writes the link into the box the sheet commits, an `@` mid-word is still an `@` |
| `test/browser/cases.mjs:571` | a region becomes the link and **never reaches the filter** |
| `test/browser/drive.mjs:96` | `BREAK=refer-veil` turns the first red |
| `test/browser/cases.mjs:598` | `@` in the title editor links into the title, drawing no body line |
| `test/browser/cases.mjs:626` | `ESC` while filtering drops the edit and stands on a row |
| `test/browser/cases.mjs:685` | `DEL` over the emptied filter box takes the box; the next takes a chip |
| `test/browser/cases.mjs:734` | a HELD `DEL` takes the box and stops — it reached the prose without a guard |
| `AGENTS.hs:4231-4278` | eleven `[Browser]`/`[Test]`/`[Docs]` notes: the split, the mount and its `inline` mode, the one-step `ESC`, the DEL rung the box became, the two cuts, the boundary, the region, the one box, the key claim |

Three defects the repo's own pinning caught while this landed, each a real one:
the picker at z-index 5 rendered **behind** the sheet; RET both took the row and
committed the paragraph, because taking a row shuts the picker and the next
listener then found no momentary surface up (fixed with `stopPropagation`, not
`preventDefault` alone); and `@` bound in two scopes tripped a scope-blind
"nothing is bound twice" rule, which was refined to be per-scope and to assert
the split positively.

## Risk

The write path is the sheet's existing one — no new byte route, no new lock, no
org bytes moved by anything here. `viewPage` is a pure extraction: `headlines`
is `viewPage … (const True)` and its behaviour is unchanged, which the whole
`/headlines` suite still pins.
