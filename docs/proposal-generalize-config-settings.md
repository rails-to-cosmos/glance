# Proposal — one registry for config-layer settings

**Status:** proposed · **Date:** 2026-08-04 · **Source:** generalizer sweep over
the capture-v2 territory

## Pattern

A config setting — the default filter, the capture target, now the capture
template — is one concept registered by hand at ~20 sites over 5 source files
and 2 test files. The template (member #3, capture v2) paid ~10 mechanical
registration sites beyond its ~90 lines of unique region logic, and the diff
shows each one: key constant + `settingOf`/`settingEdits` wrappers + exports
(`Config.hs:207-209,242,295,324`), `ConfigLayers` field + load population
(`:378-379,433-434`), Query re-export, `ConfigParts` field (`Query.hs:3004`),
the `configEdits` splice chain (`:2987-2988`), `parseConfigWrite`
(`Routes.hs:1041`), `configView`'s field (`:936/948`), `writeLayer.scoped`'s
system-only mask (`:1013`), `Page.hs`'s `crow`, and the Glue slot pattern —
`view/viewBase`, `cap/capBase`, `tpl/tplBase`, 6-7 sites each, third copy just
landed (`Glue.hs:3672-3673,3693,3768-3769,3790-3811`).

Growth rate is real: three members in two months.

## The two unguarded copy-points (findings on their own)

1. `writeLayer.scoped` masks tree-wide settings out of tag-layer writes by a
   hand-written record update (`p { cpFilter = Nothing, cpCapture = Nothing }`).
   A new tree-wide setting omitted from it lets a tag-layer write set a
   tree-wide value silently; the only guard is the per-setting hand-written
   test pair (`TestServe:8870,8905`). No test quantifies over `ConfigParts`
   fields.
2. Two consumer paths answer one question: `clFilter`/`defaultFilter`
   (store-cached, `Config.hs:392`) beside `servedFilter`/`systemSetting
   defaultFilterOf` (route-fresh, `Routes.hs:948`). `clX` is itself
   `systemSetting xOf files` at load, so member #4 must choose a path or
   implement both, and nothing says which.

## Proposed change

A `Setting` registry in `Data.Org.Config` — one row per member carrying what
varies and nothing else:

```haskell
data SettingScope = TreeWide | PerLayer

data Setting = Setting
  { setKey    :: !Text          -- ^ the pragma key, or the region name
  , setScope  :: !SettingScope  -- ^ what writeLayer's mask reads
  , setKind   :: !SettingKind   -- ^ line pragma vs region (template)
  }

settings :: [Setting]           -- the ONE list
```

Readers derive from the list: `scoped` folds it (a `TreeWide` row is masked
from tag layers by the fold, not by a hand edit), `ConfigParts` stays a record
(the compiler-checked half is right) but a completeness test zips its fields
against `settings`, and the settings sheet's Glue slots become a walked list
(`CFIELDS`) the way `SECTIONS` already walks panels — the reuse sweep's
finding #28 is this proposal's shell half.

The consumer-path rule gets one sentence in the module doc and one enforcing
test: store-cached `clX` for the load path, `systemSetting` fresh reads for
routes that must not consult a stale store, and a member registers exactly one.

## Files

`src/Data/Org/Config.hs`, `src-query/Glance/Query.hs`,
`src-web/Glance/Web/Routes.hs`, `src-web/Glance/Web/Page.hs`,
`src-web/Glance/Web/Page/Glue.hs`, `test/TestConfig.hs`, `test/TestServe.hs`.

## LOC estimate

+~40 (registry, fold, quantified tests) / −~30 immediately (the scoped mask,
slot copies) / **−15 to −18 per future setting** (the measured member-#3 cost).

## Risk

No wire change (`/config`'s shape stays). Test baselines move where the
hand-written mask pair becomes a quantified case. The Glue slot-list half
touches the settings sheet's dirty tracking — behavior-pinned by the existing
sheet tests.

## Existing precedent

`SECTIONS` (the settings panels as one list), `commands` (one table,
`commandNames = map fst`), `keywordScopes` (one chain, three readers),
`viewColumns` (four-way lockstep off one list).
