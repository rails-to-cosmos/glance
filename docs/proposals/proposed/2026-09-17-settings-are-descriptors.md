# Proposal — settings are typed descriptors

**Status:** proposed · **Date:** 2026-09-17 · **Origin:** flat catalogue maintainability audit

## Pattern

`settingsRows()` creates a visible row and separately inserts a partial model
into `settingModels` (`frontend/glue/50-settings.js:51-57`). Editing later
rejoins the two structures by id and detects asynchronous writes by looking for
a `.then` member (`frontend/glue/50-settings.js:152-176`). The positional
`add` call carries eight arguments, so each local preference, saved view,
layer value and hue repeats the same assembly in a different shape
(`frontend/glue/50-settings.js:58-131`).

The same file now has three owners: settings/configuration at lines 1–499,
main view navigation at 500–700, and browser preferences/key hints at 701–809.
A move-only file split would preserve their ambient coupling.

## Proposed change

Make one descriptor the model and derive rows from it:

```ts
type SettingState =
  | "saved" | "changed" | "syncing"
  | "conflict" | "error" | "read-only";

interface SettingDescriptor {
  id: string;
  label: string;
  area: string;
  appliesTo: string;
  source: string;
  read(): string;
  state(): SettingState;
  commit?: (raw: string) => void | Promise<void>;
}
```

`settingView(descriptors)` becomes a pure mapping to the six table columns.
`editSetting(byId, id, raw)` owns validation errors and the
changed/syncing/saved/error transition once. Descriptor constructors capture
the repeated policies:

- `localSetting` for storage/native preferences;
- `viewSetting` for saved queries;
- `layerSetting` for TODO cycles and capture templates;
- `hueSetting` for theme/keyword pairs;
- `resolvedSetting` for read-only derived values.

Then expose `Preferences.settings()`, `Views.settings()` and
`Config.settings()`. The catalogue concatenates providers. This permits the
existing file to split by state ownership behind real handles:
`SettingsPage`, `Views` and `Preferences`, instead of three fragments sharing
globals.

## LOC estimate

Add about 45 lines for the descriptor type, constructors and one editor;
remove about 70–95 lines of positional row/model and repeated async-state
plumbing. A new preference becomes one 6–10 line descriptor plus its unique
codec or platform effect.

## Risk

Low. Preserve row ids, labels, source strings and commit timing. It changes no
wire or storage keys. Promise rejection and synchronous validation need paired
tests because their current paths differ.

## Existing precedent

`CFIELDS` already puts dirty comparison, wire encoding and receipt handling in
behavior-owning records (`frontend/glue/50-settings.js:290-307`). `AGENTS.hs`
already models the closed setting columns and states (`AGENTS.hs:5262-5284`).
