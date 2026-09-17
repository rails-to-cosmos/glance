# Proposal — retire Elm.Listing in favour of table-view

**Status:** done · **Date:** 2026-09-17 · **Origin:** front-end maintainability audit after widget reuse became a high-priority rule

## Outcome

Links and tags now mount the shared renderer through `popupTable`, using its
inline layout, local narrowing, cursor and ordered flag state. The sibling
renderer needed no change. `Listing.elm`, its JavaScript port mirror and its
declarations are gone; `assets/elm.js` now contains `Doc` alone. The handwritten
runtime source change removes 374 net lines outside tests and generated assets.

## Pattern

`frontend/elm/src/Listing.elm:17-300` is a second table widget: it owns columns,
rows, point, delete flags, click selection, substring narrowing, table markup,
badge cells and a filter field. The JavaScript adapter at
`frontend/glue/20-sheet.js:1391-1442` mirrors its state synchronously because
Elm ports arrive later.

Settings already moved to the real table-view mount
(`frontend/glue/50-settings.js:178-190`). Only the links and tags popups still
mount `Elm.Listing` (`frontend/glue/40-popups.js:11-28,90-117`). The reference
picker proves table-view's compact mode in this shell
(`frontend/glue/60-refer.js:147-177`).

## Proposed change

Add one adapter over the shared renderer:

```js
mountPopupTable(host, columns, { rows, flags, flagHelp, onClick })
  -> TableHandle
```

It mounts table-view in `inline` or palette mode with sorting and action hints
off. Links and tags keep their domain row mapping and callbacks. Temporarily
adapt table-view's filter and count API to the `narrowing`, `openNarrow`,
`shutNarrow`, `narrowBox` and `counted` vocabulary expected by `popupKeys`.
If a small capability is missing, add it to `~/sync/stuff/table-view` and vendor
the renderer after the spike.

Then delete:

- `frontend/elm/src/Listing.elm`;
- `ListState` and `ListPorts` from `frontend/glue.d.ts`;
- `listing()` from `frontend/glue/20-sheet.js`;
- the Listing interception in `test/fixtures/shell-harness.js`.

Keep behavior coverage for focus on opening the narrow, flag order, landing,
click selection and empty lists. Stop asserting the removed port bridge.

## LOC estimate

Delete about 325 Elm lines, 53 JavaScript bridge lines and 19 declaration
lines. Add roughly 30–50 adapter lines. Immediate net reduction: about 350
lines. The next small tabular surface supplies columns, rows and callbacks; it
adds no renderer, decoder or port model.

## Risk

Medium. The visible contract is already table-view's styling, but focus timing,
flag retention and compact geometry must be compared in the browser spike.
There is no server wire, org or persistence change.

## Existing precedent

Settings uses table-view as its full-page catalogue
(`frontend/glue/50-settings.js:178-190`). The reference picker uses its compact
inline mode (`frontend/glue/60-refer.js:158-176`). This proposal revises the
older choice in `docs/proposals/partial/2026-08-09-elm-sheet.md`, made before
widget reuse became a high-priority product rule.
