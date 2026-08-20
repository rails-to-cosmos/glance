# Bug — an `id:` link resolves over `ORG_GLANCE_ID`, org-id's property unread

**Status:** fixed · **Reported:** 2026-08-20 (user review of the open-link
proposal) · **Surface:** `ref:` filtering; the open-link proposal inherits it

## The symptom

An org-id link — `[[id:UUID]]` written by `org-store-link`, naming a
headline's `:ID:` property — never resolves to that headline in glance. And
worse than a miss: if any row's `ORG_GLANCE_ID` happens to equal the UUID,
the link resolves to *that* row instead. `id:` is org-id's protocol; glance
reads it as its own key.

## Steps to reproduce

1. A tree with headline A carrying `:ID: deadbeef-…` (org-id's property, no
   `ORG_GLANCE_ID` needed) and headline B whose body links `[[id:deadbeef-…]]`.
2. Query `ref:<A's row id>` — B is not served.

## The evidence

- `src-query/Glance/Query.hs:561-562` — `refPrefixes` carries `"id:"`, so
  the target is parsed as a row reference;
- `src-query/Glance/Query.hs:617-618` — `refSpellings` answers only
  `identity` (`ORG_GLANCE_ID`) and the title; the `ID` property is read
  nowhere;
- `src-web/Glance/Web/Filter.hs:243-244` — the match is
  `refTarget ∈ rrTargets`, so an org-id UUID matches nothing of A's.

## The fix (same day)

The namespace rides the reference: `Ref` gained `refVia` (`ViaRow` /
`ViaOrgId`), set by `refTargetOf` from the prefix; `RefRow` carries the
row's `:ID:` via `orgIdOf` (`orgIdentity`, org-id's twin of `identity`);
`keyTest Ref` matches each link in its own namespace. Pinned in
`TestFilter` (`Org row`/`Crossed` fixture rows). The open-link proposal's
"`id:` law" section carries the same rule for `link.row`.
