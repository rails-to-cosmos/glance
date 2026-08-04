# Proposal — one surface list, many readers

**Status:** proposed · **Date:** 2026-08-04

Doctrine names `SURFACES` in the same breath as `viewColumns` and
`keywordScopes` — "the order is spelled once and every consumer reads it"
(`docs/design-rhymes.md:82-87`).  It is one of the three that does not hold.  A
fifth overlay costs seven registration lines across three files, none of them
enforced, and the fifth overlay is already here: the settings sheet has markup, a
z-band and a size tier, and never joined the list.

## The seven sites

`SURFACES` (`src-web/Glance/Web/Page/Glue.hs:4109-4116`) carries four entries —
`prompt`, `links`, `tags`, `sheet` — and is walked by four readers: `momentary`
(`:4119`), `sole` (`:4133`), `typing` (`:4141`) and `cancel` (`:4311`), plus
`popupKeys`' DEL branch (`:4497`).  It names each surface's `up`/`off`/`edit`/
`shut` and never its DOM id, so the CSS cannot read it and `sole()` cannot raise
anything.

The stylesheet therefore rolls the same five ids by hand in six places:
`Style.hs:225` (the backdrop), `:229` (`.on` → `display:flex`), `:518` (the box's
`z-index:101`), `:522-523` (head and foot chrome), and `:681` plus `:682-683` —
BOTH stale-wash selectors.  The markup adds a wrapper and a box
(`Page.hs:93-130`), and the suite keeps a hand-written five-entry tier table
(`test/TestServe.hs:5101-5102`).

## What omission costs

`typing()` is the predicate that kills every `table`-scope key while an overlay
is up.  A surface left out of `SURFACES` leaves `d` (`archive-flag`) and `D`
(`org-glance-overview:delete`) live underneath it.  That is the failure mode the
file already knows about — `Glue.hs:4130-4132` says a hand-written list "was a
fourth registration site whose omission failed silently", which is why `sole`
reads the list.  Joining the list stayed unchecked.

`#config` demonstrates it.  It is absent from `SURFACES`, so `live`
(`Glue.hs:4150-4152`) special-cases it with a second predicate:

```javascript
|| (b.scope === "modal" && (editing !== null || settings))
```

`typing()` (`:4143`) does not see the settings sheet as up at all; it is caught
by the focused-field branch instead, which is a different rule reaching the same
answer by luck.

Omitting `:681-683` costs the stale wash on that overlay alone — invisible until
a fetch runs past its grace period.  The suite pins only the `sheet` entry's text
(`test/TestServe.hs:5702-5703`); `prompt`, `links` and `tags` are unasserted,
nothing counts entries and nothing checks order, though `Glue.hs:4128-4131` says
order is load-bearing for the `+`-over-tags case.

The one thing that cannot drift is the SIZE, because the tier is a class
(`Style.hs:659-660`) and doctrine already made a surface declare no width of its
own.  That is the shape the rest of the surface wants.

## Proposed change

Move the list into Haskell and emit both halves from it, the way `keyBindings`
is emitted into `keyBindingsJSON` (`Keymap.hs:322-334`).

```haskell
-- | The overlays this page raises, in the order 'cancel' and @momentary()@ walk
-- them.  ONE list: the markup wrapper, the six selector rolls in
-- 'Glance.Web.Page.Style' and the @SURFACES@ blob the glue parses are each a
-- comprehension over it, so a fifth surface is one entry where it was seven
-- lines nothing checked.
data Surface = Surface
  { sfId        :: !Text        -- ^ the wrapper's DOM id; the backdrop wears it.
  , sfBox       :: !Text        -- ^ the inner box's id.
  , sfTier      :: !Text        -- ^ @pop-band@ or @pop-sheet@; no box declares a width.
  , sfMomentary :: !Bool        -- ^ raised OVER a sheet to answer a question.
  , sfUp        :: !Text        -- ^ the glue predicate saying it is up.
  , sfOff       :: !Text        -- ^ the glue function that drops it.
  , sfEdit      :: !(Maybe Text) -- ^ its in-place edit predicate, where it has one.
  , sfShut      :: !(Maybe Text) -- ^ what cancels that edit.
  }

surfaces :: [Surface]
surfaces =
  [ Surface "prompt" "pbox" "pop-band"  True  "() => !!prompting" "unask"  Nothing Nothing
  , Surface "links"  "lbox" "pop-sheet" True  "linking"  "shutLinks" (Just "lediting") (Just "cancelLinkEdit")
  , Surface "tags"   "tbox" "pop-sheet" True  "managing" "shutTags"  (Just "renaming") (Just "cancelRename")
  , Surface "modal"  "sheet" "pop-sheet" False "docHolds" "shut"     (Just "sheetOpen") (Just "cancelSheetEdit")
  , Surface "config" "cbox" "pop-sheet" False "() => settings" "shutSettings" Nothing Nothing
  ]
```

`Glance.Web.Page.Style` then reads it:

```haskell
selector :: (Surface -> Text) -> Text -> Text
selector f suffix = T.intercalate "," [ "#" <> f s <> suffix | s <- surfaces ]
```

so `:225` is `selector sfId ""`, `:229` is `selector sfId ".on"`, `:518` is
`selector sfBox ""`, and the two stale-wash rolls at `:681-683` are the same
call.  The markup at `Page.hs:93-130` becomes a comprehension emitting
`<div id="X"><div id="Xbox" class="TIER">`, and `SURFACES` is emitted from the
same list beside `keyBindingsJSON`.

The CSS collapses further once every wrapper wears a class, which is what
`Style.hs:441-444` already argues for in prose ("all four share every
declaration but geometry"):

```css
.pop{display:none;position:fixed;inset:0;z-index:100;background:#0009;…}
.pop.on{display:flex}
html.stale .pop,html.stale #app{opacity:.55}
```

## The boot-time check that comes free

`SECTIONS` (`Glue.hs:3494-3498`) already has the enforcement this family lacks:
`sec.appendChild(el(id))` at `:3503` throws on an id the markup does not carry,
and `:3492-3493` calls that out — "a join like that should fail at boot".  A
`SURFACES` emitted from Haskell can do the same: one `el(s.id)` per entry at
mount, so a surface whose markup went missing fails on the first paint rather
than by leaving `d` live under an overlay.

## LOC

Added ~20 (the record, the list, `selector`, the markup comprehension).  Removed
~26 (six selector rolls collapse to six calls plus three class rules; `live`'s
`|| settings` special case; the hand-written wrapper markup).  Saved per future
surface: seven unenforced registration lines become one list entry.

## Risk

The page must stay byte-identical, which is checkable — `TestServe`'s existing
page assertions and the tier table at `test/TestServe.hs:5101-5102` are the
oracle, and the tier table should become a comprehension over `surfaces` only
AFTER the byte-identity is confirmed with it hand-written.  No wire field moves,
no route moves, no keymap moves.  Adding `#config` to `SURFACES` is a real
behaviour change (it makes `typing()` see the settings sheet directly), so it
should land as its own commit with the `live` special case removed in the same
step.

## Existing precedent

`Glance.Web.Keymap.keyBindingsJSON` (`Keymap.hs:322-334`) — a Haskell list of
records emitted into a JSON blob the page parses for its own dispatch, so a key
cannot be bound and undocumented.  `SECTIONS` (`Glue.hs:3494-3503`) for the
boot-time join check.  `Style.hs:659-660` for the tier, which is the half of this
that already works.
