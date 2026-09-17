# Proposal — completion menu is a widget

**Status:** done · **Date:** 2026-09-17 · **Origin:** glue cross-cut audit

## Pattern

The material sheet defines generic completion-list state, painting, movement,
taking and viewport placement in `frontend/glue/20-sheet.js:858-1015`.
`frontend/glue/37-tags.js:86-184` then reaches those functions through the
ambient script scope for the tag and state cell menu. Dates, properties, tag
runs and draft states differ in vocabulary and how a chosen value is applied;
their list interaction is the same.

The abstraction currently lives inside a 1,904-line feature file even though
several later parts consume it. That placement is one source of the load-order
cycles that prevent factory boundaries.

## Proposed change

Extract one explicitly constructed adapter:

```js
createCompletionMenu({ element, anchor, renderItem, apply }) -> {
  setItems(items, initialIndex),
  move(step),
  take(field),
  place(),
  close()
}
```

The widget owns `items`, point, DOM painting, above/below placement and close.
Each feature owns its vocabulary, filtering and value transformation. The date
box can keep its own menu element while using the same constructor; the shared
root menu used by tag and state fields becomes another instance.

Keep synchronous DOM and caret work in JavaScript. This extraction is an
enforced factory boundary, not an Elm port: a port round trip would make offer
movement and field replacement asynchronous.

## LOC estimate

Mostly a move, followed by about 15–30 lines removed from repeated wiring. A
new completion source supplies items, rendering data and an apply function in
roughly 10–20 lines instead of acquiring its own mutable list, cursor, paint,
walk, take and placement logic.

## Risk

Low to medium. Geometry is sensitive, but behavior stays synchronous and the
browser suite covers under/over placement, offer walking and taking. Preserve
the current rule that ESC belongs to the containing surface.

## Existing precedent

`Palette`, `Popups` and `Keys` are already factories with explicit dependency
lists. The menu should become the next low-level factory because its consumers
already cross feature files.

## Outcome

`frontend/glue/18-completion-menu.js` now owns offer state, rendering, movement,
taking, closing and viewport placement. The property, date and shared tag/state
menus construct separate instances and provide only their vocabulary, anchor
and field transformation. The old cross-file helpers and mutable menu records
were removed from the material sheet.

The widget remains synchronous JavaScript so a taken offer updates the field
and caret during the key event. Type checking covers its public contract;
focused shell and Chromium cases cover completion, unchanged takes, movement,
and below/above placement.
