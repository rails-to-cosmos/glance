# Spike — settings in the main table

**Date:** 2026-09-17 · Design spike for review. It is not wired into the app.

Open `index.html`. The three buttons switch information architectures. Press
`,` to move between the fixture main view and settings. Inside settings, use
`n`/`p` or the arrow keys, `RET` to enter a row or edit a value, `DEL` to move
up, and `/` to narrow the current table. The breadcrumb in the shell header is
clickable.

## Shared rule

Settings is key-value data. It replaces the main table at the existing mount
and is rendered by the vendored `assets/table-view.js`, rather than a
settings-specific table imitation. Every settings screen gets table-view's
cursor, row activation, narrowing, and inline value editing. The shell header
owns the global breadcrumb:

`@ views:main › settings › …`

The spike adds no backdrop, sheet, tab strip, or settings-only navigation. The
material document remains the one overlay. This spike changes settings only.

## Variants

| Variant | First settings table | Depth | Best quality | Cost |
|---|---|---:|---|---|
| **A · Flat catalogue** | Every setting | 1 | Fast global scan and narrow | Unrelated settings share one long table |
| **B · Category drill** | Interface, Views, Keywords | 2–3 | Clear entry point and room for growth | One extra activation for most values |
| **C · Source drill** | Browser, Tree, config layers | 2 | Ownership and precedence are visible | Users must know where a setting lives |

## Decision

**A · Flat catalogue** was selected in review. Settings is naturally key/value
data, and one table keeps every setting available to table-view's existing
sorting and narrowing without another navigation level.

The catalogue keeps source as a column because similarly named settings can
belong to the browser, the tree, or a tag layer. Further columns should carry
the context that flattening removes while preserving the one-table model.

## Coherence check

- **Special case:** the current settings sheet owns tabs, backdrop behavior,
  focus rules, and settings-specific keyboard handling.
- **Uniform rule:** a navigable collection is a table-view route in the main
  mount; settings is the key/value instance, and the shell header reports that
  route as breadcrumbs.
- **Dissolves:** settings sheet lifecycle, settings tabs, modal focus trapping,
  backdrop dismissal, and a second navigation vocabulary.
- **Moves:** opening and closing become route transitions; values become table
  cells; sections and config layers become ordinary drill rows.
- **Escape hatch:** the material document keeps its overlay because it presents
  a document beside the working table rather than replacing a collection.
- **Verdict:** the settings sheet is removable. No settings behavior found in
  the spike requires a custom popup widget.

## Production boundary

After a variant is accepted, the build should route `openSettings()` into the
main mount, put the global breadcrumb in `#ghead`, and express settings rows
through table-view. The existing config fetch/save endpoints can remain. URL
history should encode the selected settings route so reload and Back restore
the same table.
