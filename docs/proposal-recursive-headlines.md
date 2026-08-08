# Proposal — recursive headlines

**Status:** proposed · **Date:** 2026-08-08

`fixme.org` item 9: the material sheet lets the ROOT headline change state
and tags and refuses the same of every headline under it.

## The inconsistency, exactly

The sheet's head line carries four cells — `state | priority | title |
tags` (`Routes.cells`).  `RET` on the state cell raises the value palette,
`RET` on the tags cell raises the tags popup, `t` and `:` ask the same at
the element, `S-<up>`/`S-<down>` cycle the priority.  On a CHILD line each
answers `a child is not settable yet — DEL opens its parent`
(`assets/glue.js`, `atElement`).  The invariant states it as landed
behaviour:

> A CHILD's cells are read-only in v1 — no row id, so no `/command`
> addresses it — while its planning, drawer, paragraphs and children are
> all editable through the lens that materialized it.  (CLAUDE.md, UI)

The guard is `r.kind === "child" || editing.child !== null`, so it covers
the second half: materialize INTO a child with `?child=K` and that child
becomes the sheet's HEAD line, drawn with its own stars and cells, and its
cells stay refused.  Everything else at that focus works — planning rows,
drawer, paragraphs, `SPC` on a checkbox, the whole-subtree commit.  The
sheet is a recursive editor of every byte except those four cells.

## The cause is structural

`/command` addresses ROWS.  A row is a TOP ENTRY — `recordsOf` keeps
`topLevel h = levelOf h == 1` and drops the rest — and a row id is
`ORG_GLANCE_ID`, else `FILE#K` with K the headline's place among its file's
EMITTED ROWS (`rowId`, `rowIdIn`).  A child is in neither list, so
`headlinesIn (storeRecords st) (cmdIds cmd)` has nothing to hand
`planCommand` and `set-state` has nothing to name.

`?child=K` addresses a child for READING and for the subtree WRITE, and for
nothing else: `focusIn` resolves `(rid, child)` for both `GET` and `POST
/headline`, and `commit` splices `[(hrSubtree here, org)]` under
`hrDigest here`.  The child is already addressable, already writable, and
unreachable from the one route that edits a cell.

## Three designs

### (A) Extend `?child=K` to `/command`

`CommandSpec` grows `csChild :: Bool`; the request grows an optional
`child` beside `id`; `planCommand` grows one step — `subtreeEntries` the
row, `subtreeEntryAt` the index, hand `seRecord` to the same `RowEdits`.

Cheap because three things already hold.  `RowEdits = ConfigLayers -> Maybe
Text -> Args -> HeadlineRecord -> Either Text [(Span, Text)]` takes a
RECORD, and every edit in the table (`setStateEdits`, `setPriorityEdits`,
`setTitleEdits`, `setPlanningEdits`, `addTagEdits`, `removeTagEdits`,
`renameTagEdits`, `editLinkEdits`) reads spans off `headlineSpans r` and
text off `hrDoc r`, so depth is nowhere in them.  `subtreeEntries` already
builds a full `HeadlineRecord` per descendant through the same `recordOf` —
the child's own `hrSubtree`, cells and sub-spans beside the ROW's `hrFile`,
`hrDoc`, `hrDigest`, `hrKeywords` and `hrDeclared` — and already spells an
id, `hrId = ROW/K`, decorative by its own docstring: "Nothing registers one
and no route resolves one: they exist so a refusal names something a reader
can place."  And the write law survives untouched: `planCommand` groups by
`hrFile` and pins `hrDigest r0`, which a child record carries by
construction, so a marked set is still one `replaceSpans` per file.

Costs, honestly.

1. THE ADDRESS IS POSITIONAL.  K indexes `subtreeEntries`' document-order
   list, so it moves when a headline is inserted, removed or reordered
   anywhere above it INSIDE the subtree — more volatile than `FILE#K`,
   which moves only when the file's emitted rows move.
2. THERE IS NO DIGEST PER CHILD, and there cannot be: `hrDigest` is the
   FILE's, one lock for the whole file.  It covers every concurrent EDIT —
   a byte moving re-digests the file and `planCommand`'s `stale` refuses
   that id with nothing written, so a stale K never reaches a headline it
   was not measured against.  It fails to cover a request that pins
   NOTHING: `digests` is optional (`o .:? "digests"`) and `stale` reads
   only the ids the client pinned.  For a ROW id that is tolerable, the id
   naming one headline whatever moved; for a positional child it is a write
   aimed by a number nobody checked.  ONE GUARD FIXES IT — a `child`
   command REQUIRES the row's digest, refused in `csArgs`, already handed
   the ids.
3. `child` beside more than one id is a 400 — `wantsLink`'s own rule, for
   its reason: an argument describing one row's structure means nothing to
   a second.
4. It puts a second addressing scheme on `/command`.  `(id, child)` keeps
   it the SAME scheme `/headline` uses; the `ROW/K` string would give `/` a
   rule and break "Nothing parses an id apart (`resolveIds` is
   exact-string), so the separator carries no rule."

### (B) Every headline is a row

Widen `topLevel` in `recordsOf`.  The invariant:

> A ROW IS A TOP ENTRY.  `recordsOf` keeps `topLevel` headlines and drops
> the rest; the filter runs AFTER `subtreeSpans`, and that order is kept
> because `subtreeSpans` is the outline rule over a DOCUMENT — widen
> `topLevel` and filtering first ends a deeper row at the next KEPT
> headline instead of the next shallower one.  (CLAUDE.md, Walk)

Read precisely, the extent hazard belongs to the ORDER: `outlineEntries`
computes `subtreeSpans` over every headline and `recordsOf` filters the zip
afterwards, so a wider predicate keeps correct extents while that order
stands.  What widening costs is that extents stop TILING and start NESTING
— a parent's `hrSubtree` CONTAINS its children's — and everything
downstream reads a subtree.  `hrLinks`/`hrLinked` are one scan of
`sliceSpan doc subtree`, so a parent's reference list becomes the union of
every descendant's: `ref:X` answers with the whole ancestor chain of the
entry that points, and `linked` underlines every ancestor.  `customCell`
reads a property a descendant carries.  `/headline` serves one range as
three sheets.

The numbers are measured, because this is a REVERSION.  Narrowing to top
entries took ~/sync store rows 12875 → 10685 (−17%) and id collisions 9 → 7
while `scan`'s headline count stayed 12884 and its collision tally 9;
~/sync/views went 7370 → 6084 and 2 → 0 (docs/invariants.md, "A row is a
top entry"; docs/plan-org-console-web.md).  The corpus stands at ~12.6k
headlines.  So (B) buys back +20% rows and two id collisions.

`hrSearch` is the row's own cells and does not double, so the growth is one
row per child that has a cell.  ORDINALS get far more volatile: `FILE#K`
numbers EMITTED rows, so inserting a child anywhere renumbers every row
behind it in the file, and `ORG_GLANCE_ID` is the only immunity the
corpus's children mostly lack.  `GET /tags`' `counts` is per-request over
ROWS, so every count changes; `stTags` counts FILES and holds.  And the
table becomes a list of LINES where it was a list of ENTRIES — the outline
is what the sheet exists to show, and flattening it into the table deletes
the distinction that makes materialize worth opening.

(B) genuinely buys one thing: every headline gets a row id, `?child=K` and
`csChild` both go away, and there is exactly ONE addressing scheme — item
9's goal, reached by making the table pay for it.  REJECTED.

### (C) A child is a reference of type `child_of`

The fixme's own idea.  A child becomes an addressable thing with an id of
its own and an EDGE to the entry above it, so the outline is a graph the
table already filters: `ref:ROWID` is "every row whose subtree points at
the row named", and `child_of:ID` is that question over a structural edge
rather than a textual one.

What would an id for a child BE?  Four candidates, one survivor.  `ROW/K`,
the positional address `subtreeEntries` spells, renumbers — the thing an id
exists to avoid.  The character offset of its stars WAS the row id once and
moved on any edit above the headline (`rowId`'s note).  A digest over its
text collides between two identical children and changes on every edit to
the thing it names.  `ORG_GLANCE_ID` is stable across every edit, already
the row rule, and the only one an org file can SPELL: `refSpellings` reads
the id off the HEADLINE rather than off `hrId`, "an ordinal is this view's
own invention and no file can hold a link to one", so a `ROW/K` address is
unreachable from a link by construction.

So (C) implies MINTING `ORG_GLANCE_ID` into child headlines, a write into
every file it touches.  The convention is established here: this repo mints
one per stored entry on every tagged capture (`mintBlobId` is
`org-id-uuid`'s form), `blobDocument`'s `drawerEdits` is the span math
already — join an existing `:PROPERTIES:` under its own indentation, else
write the drawer whole under the PLANNING LINE — and a minted id is
invisible in the panel by construction, `hiddenProperties`' first entry
being `ORG_GLANCE_ID` and `hiddenProperty` folding a key while knowing
nothing about depth.  Against it: the write is unasked-for, it costs one
write per child ADDRESSED, and an id in a file is permanent.

(C) buys past (A) a name that survives a reorder, an outline that is a
first-class graph, `ref:`-shaped filtering over structure, and one edge a
future refile can read.  It solves nothing alone: resolving an id to a
record needs the store to HOLD entries — (B)'s index without (B)'s rows,
`recordsOf` still emitting top entries while a second projection keys every
entry by id.  That projection is (C)'s real cost and the reason it is v2.

## Recommendation

(A) for v1, (C) as the shape v2 grows into, (B) rejected on the extent and
row-count argument above.  (A) is the smallest thing that removes the
inconsistency a reader SEES: four refusals become four commands, and one
optional field joins a route that already resolves `(id, child)` twice.  It
settles nothing about identity, which is correct — the id question is worth
answering once, with minting, rather than twice.  Staged:

1. `csChild` per entry, `child` on the request, the digest guard.  The
   glue's `atElement` refusal becomes a call carrying `editing.child`.
2. The CHAIN FIX (Decision 1): `settableStates` over a child record today
   asks a question missing the row's tags.
3. `edit-link` narrows its wall from the ROW's subtree to the focus's — a
   tighter version of the check it already makes.
4. v2: mint on demand (Decision 4), key a second projection by id, add
   `child_of` as a filter key beside `ref:`.

## What "truly recursive" means, as invariants

1. ONE ADDRESSING SCHEME FOR EVERY ENTRY.  `(id, child)` is it, `child`
   absent being the row — `Focus`'s own rule, already total (`focusHere`).
   No route grows a second spelling, and `ROW/K` stays a string a refusal
   prints rather than one anything parses.
2. A COMMAND'S LEGALITY IS A PROPERTY OF THE ENTRY, never of its depth.
   `csChild` is transitional and every `False` in it owes a reason about the
   COMMAND (`capture` makes a row; `archive` addresses the table).  A
   refusal reading "because it is deep" is the bug this proposal is about.
3. EVERY WRITE IS STILL ONE FILE, ONE DIGEST, ONE ATOMIC `replaceSpans`.
   Unchanged by construction: a child record carries the row's `hrFile` and
   `hrDigest`, so `planCommand`'s grouping and lock need no edit.  A child
   command additionally REQUIRES the pin, its address being positional.
4. ONE OWNER PER BYTE SURVIVES, AT EVERY FOCUS.  The lens's three regions
   are the FOCUS's own planning line, drawer and logbook, and every other
   byte is the body's — so a child's drawer is body text at the PARENT and
   a drawer at the CHILD.  Two focuses over one range, held apart by the
   FILE digest: the second write drifts and re-materializes.  The cost is
   stating it, plus reading `ownLines` as the general rule it already is —
   it answers "how much of this body is mine" at whatever focus asks.
5. WHAT A READER IS OFFERED IS WHAT A WRITE TAKES, AT EVERY DEPTH.
   `settableStates` is `keywordSources` flattened so the palette and the
   wall cannot come apart; a child's cell must raise a palette asking
   `GET /keywords` about the CHILD, under the chain Decision 1 settles.

## Open decisions

1. **`set-state` on a child: the ROW's chain or its own?**
   `keywordSources` builds its chain from `mergeKeywords (map hrDeclared
   rows)` and `tagsOfCell (hrTags r)` — the file's `#+TODO:` plus the
   RECORD's own tags.  A child record from `subtreeEntries` carries the
   file's declarations and the CHILD's own tags cell, so `settableStates`
   over it today resolves a chain missing every tag the row wears.
   RECOMMEND the ROW's: the chain is the FILE's plus the ROW's tags, org's
   `org-use-tag-inheritance` makes the row's tags the child's, and the
   palette in front of the reader at that sheet is the row's.  Spell it as
   the row's tags UNION the entry's own — one function over the path
   `trailTo` already walks — so a child adding `:book:` reaches the book
   layer's cycle and a child adding nothing keeps the row's.
2. **What `archive` on a child means.**  `archiveEdits` IS `addTagEdits
   archiveTag`, and `/headlines` hides a row by `-tag:*archive*` over the
   ROW's own tags cell.  Tagging a child hides nothing: the row stays on
   the table carrying an archived child.  RECOMMEND `csChild = False` for
   `archive` in v1, the refusal naming the reason — the word means "take
   this off the table" and a child is not on it.  Revisit under (C), where
   archiving a subtree is an operation over an edge.
3. **Do a child's tags inherit the row's for filtering?**  Two directions,
   asymmetric.  DOWNWARD — the row's tags count as the child's — is org's
   own rule and costs the table nothing, no child having a row.  UPWARD — a
   child's tag makes the ROW match `tag:x` — changes what `/headlines`
   answers, changes `GET /tags`' per-request counts, and breaks the
   top-entry rule's first stated consequence, that a word only a child
   carries matches nothing.  RECOMMEND downward only, and only where a
   CHAIN is resolved (Decision 1); `tag:` keeps reading the row's own cell.
   Upward, if ever wanted, is a NEW key over the graph the way `ref:` is.
4. **May the daemon mint an id unasked?**  RECOMMEND no.  Minting is a
   write and every write here is a reader's act.  Mint on the FIRST command
   that addresses the child, inside the SAME atomic `replaceSpans` as the
   edit it arrived with — one file, one digest, one write, the drawer
   splice being `blobDocument`'s `drawerEdits` already.  Never on a walk,
   never on a materialize, never on a read: a reader who only looked leaves
   no byte behind.
5. **Does `?child=K` stay once ids are minted?**  RECOMMEND both.  K is how
   a client WALKS the outline — `children`, `parent` and `path` are all
   indices — and an id is how an address survives a reload.  Serve a
   child's `ORG_GLANCE_ID` in `childJSON` where it has one and let a
   command name either, with `focusIn` the one resolution both go through.
