# Proposal — a relation is a link with a kind, so the link ships first

**Status:** proposed · **Date:** 2026-08-15 · **Origin:** user, framing stage 5b
of [`2026-08-12-relations.partial.md`](2026-08-12-relations.partial.md) as a
layering: *popups → org-links in the material sheet → relations on top, because
relations are links.*

This is an IMPLEMENTATION plan, and it takes no model decisions. The protocol,
`?kind=`, id-only resolution and the reverse index are settled in
`2026-08-12-relations.partial.md`; every one of them is a dependency here, never
a question. What is new is the decomposition, the widget inventory, and the
interaction spelled key by key so the UX can be argued with before anything is
built.

## The layering

| layer | what ships | what it needs |
|---|---|---|
| **0** | nothing — the popup machinery already exists | — |
| **1** | `@` in the sheet inserts a **plain org link** to a headline | the picker; `GET /refer` without kinds |
| **2** | the same popup grows an optional **kind** field | stage 1 of the relations proposal (`Ref`, `refKind`) |

Layer 1 is shippable and useful alone: the corpus says 42% of rows carry no
`ORG_GLANCE_ID` and nothing can link to them, but the other 58% are exactly what
a reader wants to point at from a sentence. Layer 2 is one field and one query
parameter on top, because **a relation is a link whose target carries
`?kind=SLUG`** — the whole of the difference.

## Layer 0 — what already exists, and is therefore not built

Everything below is cited because the implementer should reuse it rather than
re-derive it.

### The surface registry

`frontend/glue/70-shell.js:17-37` — `SURFACES`, six entries, each declaring
`name`/`up`/`off` and optionally `open`/`edit`/`shut`/`rowed`/`narrow`/`wide`.
Nine readers walk it: `surfaceUp`, `momentary`, `sole`, `typing`, `cancel`,
`popupKeys`' DEL branch. **A seventh surface is one entry**, and it inherits
ESC-walks-out, the backdrop click, `?page=NAME` restoration and the
momentary-dismiss rule for free.

### The popup shell

`frontend/glue/40-popups.js:32,36` — `shutPopup(id, shape)` and
`showPopup(id, p, head, foot)`, the open/close pair every popup already uses.
`:65` `openOver(shape, at, none)` opens one over a row.

Two size tiers, `src-web/Glance/Web/Page/Style.hs:279-281`:

- `.pop-band` — `min(560px,100%)`, the prompt's tier (`#pbox`, `Page.hs:46`)
- `.pop-sheet` — `min(80vw,100%)`, capture's and config's

**The picker is `.pop-band`.** It is a two-field form, not a table.

The chrome — backdrop, z-band, `#…{display:none}` / `.on{display:flex}`, the
head/foot rows, the `--g-pop-max` clamp — is `Style.hs:84-90,208-220` and is
shared by id list. One id added to those selectors is the whole of the styling.

### The completion widget — and it is already the right one

`frontend/glue/30-capture.js` has a hand-rolled completion over a text input,
and its behaviour is **exactly what the kind field wants**:

- `:45` `drawTagList(typed)` — filters `capping.vocab` by folded substring,
  caps at **8 rows**, draws `#klist` with `.ke` / `.ke.kh` for the highlight
- `:87-99` — `C-n`/`<down>` and `C-p`/`<up>` walk `capping.hot`; `RET`/`TAB`
  settle
- `:60` `settleTag` — `const picked = capping.hot >= 0 ? capping.shown[capping.hot] : null`,
  and when nothing is highlighted **it takes the typed text**, folded

That last line is the match-not-required rule, already written and already
shipped. The kind field is this widget with a different vocabulary.

`#klist`, `#ktag` and their styles are `Style.hs:211-220`; the markup is
`Page.hs:58-62`.

### The write path

`frontend/elm/src/Body.elm:391` `joinAt` picks a landing by grain; `:515,:528`
`draftRow` draws the zero-width draft `+` already uses. The commit is the
sheet's existing drift-locked `POST /headline {body, properties, planning,
digest}`. **No new write route, no twelfth command.**

### The key, and the peer's split

`src-web/Glance/Web/Keymap.hs:86` already binds `@` in the **table** scope to
`org-glance-overview:relations` → `relations` (`50-settings.js:560`), which
drills into *in-edges*. The peer binds the same key twice for the same reason:
read in the overview, write in the material buffer. So the sheet's binding is a
second row in the same registry, `"modal"` scope, and the split is the peer's
own.

## Layer 1 — `@` inserts a link

### The interaction, key by key

```
  ┌─────────────────────────────────────────┐
  │ @  link to…                             │   ← .pop-band, #rbox
  ├─────────────────────────────────────────┤
  │ [ wrike mde                           ] │   ← #rq, the target field
  ├─────────────────────────────────────────┤
  │  Wrike MDE Team              3 refs     │   ← #rlist, at most 8
  │  Wrike MDE onboarding                   │
  │  MDE weekly                             │
  └─────────────────────────────────────────┘
```

| key | what happens |
|---|---|
| `@` | over the document pane, with or without an edit open, raises the picker |
| typing | re-queries after the shell's own 120 ms debounce; the list redraws |
| `C-n` `<down>` / `C-p` `<up>` | walk the highlight, `capping.hot`'s rule exactly |
| `RET` **with a highlight** | commits the link and dismisses |
| `RET` **with no highlight** | **refuses** — the target is match-required; echoes `no match`, the popup stays, nothing is typed away |
| `ESC` | dismisses through `SURFACES`, leaving the document as it was |
| `TAB` | at layer 1, same as `RET`; at layer 2, moves to the kind field |

### Where `@` may fire, which is the one real UX question

`@` is an ordinary character in prose, and the sheet's document pane is where
prose is typed. Three ways out, and **the recommendation is the third**:

1. Bind `@` only when no edit is open. Costs the case the relations proposal
   calls the right one — *"the reader is writing the sentence that explains the
   reference"* — so the reader must stop typing, press `@`, and lose the caret.
2. Bind `C-c @` while editing. Correct, and nobody will press it.
3. **Fire on `@` at a WORD BOUNDARY** — start of the line, or after whitespace —
   the convention every chat and issue tracker already taught the reader.
   `foo@bar.com` never triggers. `ESC` dismisses and **leaves the literal `@`
   standing**, so the escape hatch is the key the reader already knows.

Under (3) `@` needs no keymap row in the editing case at all: it is the
paragraph editor noticing a boundary `@`, the way the renderer's filter box
notices `|`. The keymap row is for the **non-editing** case, `"modal"` scope,
`org-glance-material:refer` → `refer`.

### Where the link lands

- **an edit is open** — insert at the caret, replacing the trigger `@`;
- **no edit is open** — `+`'s own path: `joinAt` picks the landing, a draft row
  is drawn, and the box is seeded with the link text instead of an item lead.
  A box still holding only its seed writes nothing, which is `+`'s rule
  unchanged.

### The endpoint

One row in the route registry (`src-web/Glance/Web/Routes.hs:130-133`):

```haskell
, (["refer"], True, textRefusal, [(methodGet, referView hub request)])
```

```
GET /refer?q=TEXT[&limit=N]
  { "rows": [ { "id": "…", "title": "Wrike MDE Team", "refs": 3, "insert": "[[glance:…][Wrike MDE Team]]" } ] }
```

- `q` narrows through `Glance.Web.Filter`'s own `compile` over `hrSearch` — the
  same grammar the table takes, so there is no second matcher and
  `state:TODO wrike` works in the picker.
- **Addressable rows only.** No `ORG_GLANCE_ID`, no row: the 42% wall is a
  filter, never a refusal met after choosing.
- **The sheet's own row is dropped** — a row is not its own reference.
- `insert` is composed **server-side**, so the page spells no bracket grammar
  and the peer's slug rule is applied where it is already implemented.
- `refs` is the in-degree, and is why this reads better after stage 4 of the
  relations proposal. Until then it is omitted rather than faked.

## Layer 2 — the kind

The same popup grows a second field. `TAB`/`RET` moves to it; it is **optional**
and **match is not required**.

```
  ┌─────────────────────────────────────────┐
  │ @  link to…                             │
  ├─────────────────────────────────────────┤
  │ Wrike MDE Team                        ✓ │   ← settled
  │ [ auth                                ] │   ← #rkind
  ├─────────────────────────────────────────┤
  │  author                      41 rows    │
  │  authorised-by                2 rows    │
  └─────────────────────────────────────────┘
```

| key | what happens |
|---|---|
| `RET` **with a highlight** | takes the highlighted kind, commits |
| `RET` **with typed text, no highlight** | takes the **typed text**, slugged — a new kind costs no configuration |
| `RET` **empty** | commits a plain mention, which is layer 1's link exactly |
| `C-p` off the top | back to the target field, so the ladder walks both ways |

`settleTag`'s existing line is the whole rule; only the vocabulary changes.
`/refer` grows `kinds: [{kind, rows}]`, the shape `/tags` already answers in,
folded off `refKind` once it lands — **which is the only reason layer 2 is
gated on stage 1 of the relations proposal.**

Changing the kind re-fetches `insert`, which the completion is doing anyway, so
`insert` is never stale.

### Slugging, once

The peer downcases and turns whitespace runs into `-`, on **both** encode and
decode. `foldTag` (`30-capture.js`) already does the same job for tags. The
implementer must reuse one folder rather than write a second, or `Author` and
`author` become two kinds across the wire.

## Reuse ledger — what the implementer must not rewrite

| wanted | already at | note |
|---|---|---|
| surface lifecycle, ESC ladder, `?page=` | `70-shell.js:17` | one `SURFACES` entry |
| open/close, head/foot | `40-popups.js:32,36` | `showPopup`/`shutPopup` |
| popup chrome, tiers, clamp | `Style.hs:84-90,279-281` | add ids to the selector lists |
| completion list, 8 rows, highlight walk | `30-capture.js:45,87` | `drawTagList` generalized over a vocabulary |
| **match-not-required settle** | `30-capture.js:60` | the kind field's whole rule |
| field styling | `Style.hs:211-220` | `#rq`,`#rkind` join `#ktag`'s selector |
| landing by grain, draft row | `Body.elm:391,515` | `+`'s path |
| commit, 409, digest | the sheet's `POST /headline` | untouched |
| narrowing grammar | `Glance.Web.Filter.compile` | no second matcher |
| the key's meaning | `Keymap.hs:86` | second row, `"modal"` scope |

**New code is: one route, one popup module, one keymap row, one `SURFACES`
entry, and the boundary-`@` trigger.** Everything else is a call.

## Tests

- `/refer` narrows exactly as `/headlines?q=` does over one fixture; a row with
  no `ORG_GLANCE_ID` never appears; the sheet's own row never appears.
- `insert` **reparses** to the id and description it names — `spelling`'s own
  reparse-and-compare idiom.
- Browser (`test/browser/cases.mjs`): `@` at a boundary raises the picker; `@`
  inside a word does not; `RET` with no highlight leaves the document unchanged
  and fires no `POST`; a pick seeds the draft; `RET` fires exactly one
  `POST /headline`; a seed-only box fires none; `ESC` leaves a literal `@`.
- A `BREAKS` entry per case, or none of them is evidence.
- `make interop`: an edge written here decodes through `org-glance--link-edge`
  to the `(target . kind)` it names.

## Risk

The write path is the sheet's existing one, so no new byte route and no new
lock. Two live hazards:

1. **The boundary rule is a guess about a reader's hands.** It is the one thing
   in here that wants playing with before it is fixed.
2. **`refs` reads as a promise.** Until the reverse index lands the count is
   absent, and an absent column on a picker is better than a zero.

## Open decisions

1. **Boundary-`@`, or a chord while editing.** Recommendation (3) above.
2. **Does `ESC` at the kind field commit the plain link or cancel the whole
   thing?** Recommendation: cancel the whole thing — one key, one meaning, and
   `RET` on an empty kind is already the plain link.
3. **Layer 1 alone, shipped first?** It needs no part of the relations
   proposal's stage 1 and is useful on 58% of rows today. Recommendation: yes.
