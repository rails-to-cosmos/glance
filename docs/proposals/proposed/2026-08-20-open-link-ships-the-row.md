# Proposal — a material link ships its row id from the server

**Status:** proposed · **Date:** 2026-08-20 · **Origin:** /generalizer — the
altitude lens on the open-link batch: the client re-parses what the server
already parsed.

## Pattern

Opening a material link is currently the shell's own parse:
`frontend/glue/30-capture.js` reads `CFG.material`, tests `l.type` against
it, then strips the scheme and query with a regex
(`String(link.target).replace(/^[a-z0-9+.-]+:/, "").replace(/\?.*$/, "")`)
to recover the row id `materialize` needs. The server has already done this
parse — `refTargetOf` (`src-query/Glance/Query.hs`) turns the same target
into a `Ref` for `ref:` filtering. Two parsers, one grammar; the glue copy is
the one that drifts.

## Proposed change

- `Query.hs`: `materialTarget :: Text -> Maybe Text` — `refTargetOf`
  restricted to `refPrefixes`-prefixed targets (a bare `*Title` or plain
  title must NOT mint a row id), answering the id with `?kind=` stripped.
- `Routes.hs` `linkJSON` (both call sites) gains `"row" .= materialTarget
  (olTarget l)` — `null` for a link that is not material.
- Glue: the material branch keys on `link.row` and calls
  `materialize(link.row)`; `CFG.material`, the `material()` predicate and the
  scheme regex are deleted, and `Page/Glue.hs` drops the `"material"` CFG
  entry.

**The `id:` law (user review, 2026-08-20):** an `id:` link is org-id's — it
names the `:ID:` *property*, and resolving it over `ORG_GLANCE_ID` would
conflict with org-mode. So `id:` opens the material doc only through an
ID-property lookup: the server searches rows whose `ID` property equals the
target (`getProperty "ID" . properties`, the `identity` accessor's own
shape), ships the found row's glance id as `link.row`, and ships `null` on a
miss. `ORG_GLANCE_ID` stays the key of the `glance:`/`org-glance-*:` family
alone.

The `ref:` side of the same seam is already fixed (same day —
`docs/bugs/fixed/2026-08-20-an-org-id-link-resolves-over-the-wrong-property.md`):
`Ref` carries `refVia` (`ViaRow`/`ViaOrgId`) and each link matches in its
own namespace. `materialTarget` reuses that machinery: a `ViaOrgId` target
resolves through the ID-property lookup, a `ViaRow` target is the row id
itself.

## LOC estimate

+12 server / −20 glue; the next material scheme is one `materialSchemes`
entry (this batch already derived `materialTypes` and `refPrefixes` from it)
with zero glue edits.

## Risk

Wire additive (`row` beside `type`/`target`); TestServe's material-outcomes
table re-pins; the renderer ignores unknown link fields.

## Existing precedent

`refTargetOf` itself — the server-side parse this reuses; the capture flow,
where expansion is server-side and "the page never holds template logic"
(docs/capture.md).
