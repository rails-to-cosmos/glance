# Proposal — the edit overlay finishes becoming a registry

**Status:** proposed · **Date:** 2026-08-06 · **Origin:** generalizer round
four, over the title-in-place work (`1acb820`, `a6d6076`)

## The family and what a member costs today

Five edit shapes ride one mechanism: `DTITLE`, `DPARA` (glue.js ~1680–1700),
`PROW` (~1950), `LROW` (~3539), `TROW` (~3829). `openEdit`/`shutEdit`/
`placeEdit`/`hop` are already generic over the shape — the registry is half
built. What is NOT generic is everything around a shape, so a sixth overlay
(a priority field, a date cell) costs today:

1. The shape object itself — the one part that is genuinely the member's.
2. A WHOSE predicate (`dediting`, `pediting`, `renaming`, `lediting` — four
   spellings of `edit.o === SHAPE`).
3. A commit function plus a RET branch in its surface's own listener
   (`commitDocEdit` at ~1720, `commitRow` at ~1957 behind line 2045,
   `commitLink` at 3577 behind 5033, the tags rename's own).
4. Markup in `Page.hs` (one line — fine).
5. SIX CSS family enumerations in `Page/Style.hs` — the id-lists at 435
   (base), 454 (`.on`), 459 (field metrics), 483–484 (focus), 489–490
   (`::selection`), 730 (coarse-pointer 16px) — plus the suite pins that
   mirror each list.
6. Harness registration (`TAGS`, `STATEFUL`, `FOCUSABLE`, an act verb, a
   state field) — left alone below; the stub is the page's independent
   oracle, and deriving it from the markup would blind it.

Steps 2, 3 and 5 are the unenforced, per-variant boilerplate. Nothing fails
to compile when a list is missed; the overlay just renders unstyled or its
RET does nothing.

## Move A — the CSS family becomes a class

The overlay divs gain one class (`Page.hs`:
`<div id="tedit" class="ovl">…`), and the six id-lists become single-selector
rules:

```css
.ovl{display:none;position:absolute;background:var(--g-sel)}
.ovl.on{display:flex;align-items:center}
.ovl input{font:13px/1.5 var(--dk-mono);padding:5px 12px;…}
.ovl input:focus{outline:none;border-bottom-color:var(--g-border)}
.ovl input::selection,#dpara textarea::selection{…}
```

Exceptions stay by id, exactly as they already do: `#pedit{left:0;right:0}`,
`#dpara`'s span/ground/textarea rules, `#dtitle{min-width:8em}`, `#dtin`'s
document-font override. `#dpara`/`#dtitle` opt OUT of pieces (ground, field
metrics) — under the class they keep their overriding rules and lose nothing,
since the id outranks the class. The coarse-pointer list at 730 collapses
only its overlay members (`.ovl input,#dtin` replaces three entries; `#mtext`,
`#pinput`, `#ktag` etc. are not overlays and stay named).

The suite's family pins re-pin once to the class rules — and stop growing per
variant, which is the point: today every new overlay edits six Style.hs lines
and six TestServe needles.

**LOC:** ~−10 in Style.hs now, ~−12 per future overlay (six lists + six
pins). **Risk:** none on the wire; pure page styling; the pins move once.

## Move B — a shape owns its commit

Each shape gains a `commit(row)` member holding what its surface's RET branch
does today; the four listener branches become one call:

```js
else if (k === "RET") once(() => edit.o.commit(edit.row));
```

The listeners stay — each surface still gates on its own guards
(`momentary()`, `pnav()`, `e.defaultPrevented`) — only the dispatch-to-the-
right-committer disappears. The WHOSE predicates keep their other callers
(`docOpen`, the stale-guard reads) but lose the commit ones; `dediting`'s
suite needle moves once.

**LOC:** ≈ neutral now (the branches move into the shapes), −4 per future
overlay (no new predicate-plus-branch pair). **Risk:** low; four call sites,
each already passing `edit.row` or reading it first — behavior-preserving by
inspection, and every commit path has a killing test (mutation round four
proved the DTITLE pair).

## Rejected on the way, deliberately

- **`nativeWindow`/`popupShell` skeleton merge** — two sites, two roles (the
  app window quits the loop, the pane destroys itself); a parameterized
  builder is a framework for two callers.
- **`headEnter`'s cell branch as a registry** — four arms over org's own
  closed headline-part set, each arm one line; a table saves nothing.
- **`webby` vs the shell's `followable`** — a real duplicate across a
  DOCUMENTED seam ("the engine knows no daemon"); threading the list through
  `String -> IO ()` widens the one deliberately narrow signature in the
  program for two stable prefixes.
- **Deriving the harness's `TAGS`/`STATEFUL`/`FOCUSABLE` from markup** — the
  stub is the page's independent oracle; a derived list agrees with any
  mistake the page makes.
- **`placeEdit`'s three geometry modes as shape methods** — the modes share
  the pane/border/scroll arithmetic; moving them into shapes copies it three
  times.

## Order

A then B; they are independent, so either can land alone. A is the higher
value: it deletes the only per-variant cost that is BOTH unenforced and
six-fold.
