# Proposal — a state the palette can mint, in the namespace the filter already names

**Status:** proposed · **Date:** 2026-08-16 · **Origin:** asked for directly —
`+` in the set-state popup adds a custom TODO state, and each `tag:` filter in
the global filter offers its tag as a namespace.

## What is asked

`t` raises the set-state palette (`frontend/glue/70-shell.js:154`,
`frontend/glue/30-capture.js:142-150`).  It lists what the store already
declares.  `+` there should mint a state that does not exist yet, asking for:

- **namespace** — where the declaration is written,
- **name** — the keyword,
- **active or inactive** — which side of the `|` it lands on,
- **hue**, optionally, and **one per theme** — the colour config is keyed by
  theme already, so a state minted under a light theme owes a dark hue too.

The namespace select grows with the filter: every tag the applied query names
with a positive `tag:` predicate is offered beside `system`.

## The machinery this rides on, all of which exists

- **The palette.** `ask` + `setChoices` draw the sources table and stamp one
  a–z letter per label (`30-capture.js:191-224`, `whichKeys` at `:167-180`).
  `DEL` is already a fixed entry out of the letter pool (`:220-222`) — `+` joins
  it as a second fixed key.
- **The form.** `SROW` in the settings sheet is the same four fields already:
  layer, name, group, hue (`frontend/glue/50-settings.js:179-190`, markup
  `src-web/Glance/Web/Page.hs:96-99`).  `openEdit`/`hop`/`shutEdit` are surface-
  scoped, so one global `edit` serves five of them
  (`frontend/glue/20-sheet.js:231-255`).
- **A popup raised over a popup.** `askFrom` is the precedent: `%` over the
  capture textarea raises a picker and writes back at the caret
  (`30-capture.js:239-246`, `50-settings.js:334-350`).  `sole(over)` leaves the
  asking surface standing (`70-shell.js:68-71`).
- **The write.** `writeCycle` regenerates the layer's `#+TODO:` line in JS
  (`50-settings.js:302-307`); `flushConfig` posts one `POST /config` per moved
  file (`:378-408`); the server refuses anything that is not a `#+TODO:` line
  and anything declaring no keyword (`src-query/Glance/Query.hs:1679-1696`,
  `1739-1743`).  Optimistic by digest, atomic by rename
  (`src/Data/Org/Edit.hs:195-244`).
- **The hue, per theme.** `#+GLANCE_STATE_COLORS: <theme> KW=#hex` names the
  theme in the line (`Config.hs:133-134`, `177-182`), folds per theme into
  `tsColors` (`Config.hs:243`), serves as `colors: [{theme, keyword, hue}]`
  (`Routes.hs:567-568`) and emits one `:root[data-theme="X"]` block each
  (`Theme.hs:117-121`).  Read back by `badgeColor` (`00-core.js:73-75`), which
  is what already tints the palette's own entries (`30-capture.js:212`,
  `:306-308`).  **The storage is per theme end to end; only the form is not.**
- **The reseed.** A config write nudges the watcher into a full reseed
  (`src-web/Glance/Web/Watch.hs:76-78`, `:104-111`) and a moved palette closes
  sockets with `view-changed` (`src-web/Glance/Web/Store.hs:163`, `:188-189`).

## The six things that are missing

### 1. A tag layer cannot be brought into existence

`filesIn` lists `tags/*.org` that are **on disk** (`src/Data/Org/Config.hs:306-316`),
and `writeLayer` refuses any path `readConfigLayers` did not just list
(`src-web/Glance/Web/Routes.hs:596-599`).  `system.org` is the one exception: it
is always listed, with digest `""` meaning *create it* (`Config.hs:284`,
`Edit.hs:204-208`).

So minting `HANDED` under `tag:book` fails today unless
`.org-glance/config/tags/book.org` already exists.

**Proposed:** carry the same "absent is still a layer" rule to tags.  Either

- `filesIn` also lists a layer for every tag the tree's rows carry — which makes
  `GET /config` grow with the tag vocabulary, or
- `POST /config` accepts a path under the first config dir's `tags/` whose
  basename is a valid tag, and mints it, keeping the wall for every other path.

The second is smaller and keeps `GET /config` the size it is.  It needs the
`tagText` charset check (`Query.hs:1094-1099`) applied to the basename, because
`tagOf` lowercases the basename into the tag (`Config.hs:323-324`) and a
basename org cannot reparse would make a layer nothing ever selects.

### 2. `filteredTag` finds one tag; the select wants all of them

`30-capture.js:36-43` already parses the applied query and keeps positive
`tag:` predicates over a single ordinary value, discarding metas like
`*archive*` and alternations.  It returns the **first**.

**Proposed:** `filteredTags()` returning the list; `filteredTag()` becomes
`filteredTags()[0] || ""` so capture's behaviour does not move.  The select is
then `system` followed by those tags in the order the query names them, and the
default selection is the first tag when there is one, `system` otherwise —
because the row the cursor sits on is a row that filter selected.

### 3. `default` is code, not a file

The scope chain is default 0 / system 1 / tag 2 / file 3
(`Config.hs:344-355`), and rank 0 is `builtinKeywords` — `TODO | DONE` off
`defaultContext` (`Config.hs:358-360`).  There is no file behind it, so nothing
can be written there.

**Proposed:** the select offers `system` and the tags.  Folding `default` onto
`system.org` would be a lie about rank, and a state minted in `system` already
applies tree-wide, which is what `default` was asked for.  **Flagged for
review**, since `default` was named explicitly in the request.

### 4. Nothing checks a keyword's spelling on the way in

The parser's charset is letters and `_` (`src/Data/Org/Parser.hs:143-144`) —
which is exactly why the `*empty*` meta is undeclarable.  The write path has no
`keywordText` matching `tagText`.  The only wall is indirect: a malformed word
makes `todoPragmas` yield nothing and `configEdits` answers `declaresNothing`
(`Query.hs:1685`), so the user is told the block declares nothing rather than
that `IN PROGRESS` has a space in it.

**Proposed:** `keywordText :: Text -> Either Text Text` beside `tagText`, on the
`configEdits` path, saying what a keyword may contain.  The client checks the
same charset in the form so the message arrives before the round trip.

### 5. The minted state is not settable until the store has reread

`set-state` refuses any keyword outside the row's own chain (`settableStates`,
`Query.hs:1143-1145`) — correctly: a state the row's scope does not declare is
not a state the row can hold.  The config write is what puts it in the chain,
and that is asynchronous (Watch's settle, then `view-changed`).

**Proposed:** the mint form's commit writes the layer, waits for the write's
answer, re-fetches `/keywords` for the same ids, and only then fires
`set-state` — with the palette redrawn from the new sources, so the user sees
their state arrive in the table before it is chosen.  A failed set after a
successful write leaves the declaration standing, which is the honest outcome:
the state exists, this row could not take it.

### 6. The hue form asks for one theme, and it is whichever is on

`SROW`'s `shue` field is filled from `(hues[hueTheme()] || {})[r.state]` and
committed back to the same key (`50-settings.js:186`, `:200-206`), where
`hueTheme()` resolves `auto` through `prefers-color-scheme` (`:172-177`).  So a
user editing under a light theme sets the light hue and never learns the dark
one exists; the state then falls back to its palette slot
(`--g-state-a<i>`, `Query.hs:1937-1945`) on the other theme.

**Proposed:** the mint form carries **one hue field per theme** — `light` and
`dark`, side by side, each showing the slot colour it would otherwise inherit as
its placeholder.  Both ride the one `POST /config`: `hueList` already flattens
`hues` back to `[{theme, keyword, hue}]` across every theme it holds
(`50-settings.js:273-276`), so the write path takes two entries as readily as
one.

This is the same gap in the settings sheet's states table, which minting makes
visible.  Fixing it there — a second `shue` field, or a theme toggle beside it —
is the same change and should land together.

## Shape

```js
// 30-capture.js, beside askState
const NAMESPACES = () => ["system", ...filteredTags().map((t) => `tag:${t}`)];

// A second fixed entry, out of the a-z pool, beside prompting.meta:
prompting.plus = { label: "new state", key: "+", cut: -1, fixed: true, mint: true };
```

`+` (or the entry) raises a `MINT` form shaped like `SROW` — one box, five
fields (namespace, name, group, light hue, dark hue), `TAB` walking them, `RET`
committing, `ESC` leaving the palette standing — over the palette, the way
`askFrom` does.  Commit:

1. `layerFor(namespace)` — the layer row, minting the path for a tag with none;
2. push the name into `kw.active` or `kw.inactive`, `writeCycle`, `POST /config`;
3. the hues, where given, are a **second** write, one `#+GLANCE_STATE_COLORS:`
   line per theme (`Config.hs:180-182`) and system-layer only
   (`Query.hs:1726-1729`, `Config.hs:158-174`) — so a state minted in `tag:book`
   with a colour moves two files however many themes it names.  `flushConfig`
   already posts per moved layer, so this is the existing loop;
4. re-fetch `/keywords`, `setChoices`, then fire `set-state`.

## What it costs

Client: `filteredTags`, the `MINT` shape and its markup (a fourth `#sedit`-like
box, or `#sedit` reused with a namespace field and a second hue field added),
the commit chain, and the same second hue field in `SROW`.
Server: the tag-layer mint in `writeLayer`, `keywordText`, and their tests.

## Risk

- **`GET /config` shape** moves only under the first option in §1; the second
  leaves it alone.
- **A minted tag layer is a new file in the user's tree.**  It should be minted
  only on a commit that declares something, never on opening the form.
- **`clSeed` makes any layer's keyword recognized tree-wide**
  (`Config.hs:278`, `:339-340`), so a state minted under `tag:book` is *parsed*
  everywhere and *classified* only on rows carrying `book`.  That is existing
  behaviour and worth saying in the form's hint.

## Tests this owes

- `+` in the palette opens the form; `ESC` leaves the palette standing.
- The namespace select carries `system` plus each tag the applied query names,
  in query order, and nothing for `-tag:`, `tag:a|b`, or `tag:*archive*`.
- A mint into a tag with no layer file creates `config/tags/<tag>.org` carrying
  one `#+TODO:` line, and a second mint into the same tag appends to it.
- A keyword org cannot read is refused by name, with a message that says so.
- A mint with a hue moves two files, and the badge paints the hue.
- A mint carrying both hues writes one `#+GLANCE_STATE_COLORS:` line per theme,
  and each theme paints its own — the light hue never reaching the dark page.
- The minted state is settable on the row the palette was raised from.

Existing rows to extend: `test/TestServe.hs:1173-1227` (the palette),
`:4030-4048` (`+` in the settings sheet), `:7865-7885` (colour round trip),
`:8219-8265` (`/keywords`), `test/TestConfig.hs:103-353` (layer merge).
