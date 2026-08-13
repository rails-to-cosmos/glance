# Proposal — what VSCode knows that glance could use

**Status:** proposed · **Date:** 2026-08-07 · **Origin:** user ask, researched
against VSCode's current UI surface (code.visualstudio.com/docs, 2026-08)

The filter is glance's doctrine: keyboard-first, no framework, org files as
truth, minimal chrome, movement never changes context. VSCode is mouse-capable
chrome around a keyboard core — what transfers is the keyboard core.

## Ranked

1. **The command palette — org's own `M-x`.** VSCode's `Ctrl+Shift+P` is a
   fuzzy list of every command with its keybinding beside it. Glance already
   HAS the data: the keymap blob carries every row as `{keys, command, help,
   scope}`, commands are named as elisp functions, and the echo speaks them
   verbatim — a palette is that blob made searchable. SURFACE: the value
   palette's completing-read mode (`/`'s flatten) over `kbCommand`s, raised
   from `M-x` (the one Emacs key this page does not bind; `Alt+X` reaches a
   page). Each entry shows its `seq` the way VSCode shows the shortcut, which
   is how a reader LEARNS the keys — the palette teaches, the keys stay the
   fast path. Commits by running the row's handler through the same dispatch,
   so scope rules hold for free. COST: small — no new surface, one new
   binding, the list is the blob. The discoverability win is the largest on
   this list: today the key line and `docs` are the only teachers.

2. **Sticky scroll — the subtree's own heading pinned.** VSCode pins the
   enclosing scope's opening lines at the editor's top edge. In the
   materialize sheet a long subtree scrolls the headline and the child
   headings out of view, and the reader loses WHERE they are. SURFACE:
   `#mdoc` — the headline line and child heading rows take
   `position: sticky; top: 0` (stacked offsets per depth, the way the
   renderer's own table header already sits at z 1). CSS-only against the
   existing row structure; the cursor's `keepInView` margin already knows the
   pane's line height. COST: near-zero; the one care is the selection wash
   under a stuck row. `mwhere` (the crumb line) already answers "which
   entry" — this answers "which SECTION of it", which crumbs cannot.

3. **`keybindings.json` — the rebinding layer the doctrine already
   promises.** VSCode's keybinding editor is a searchable table over
   user-overridable bindings. CLAUDE.md already commits to this ("the
   rebinding config to come will address a function by exactly this string"),
   and the pieces exist: keymap-is-data, commands as stable elisp names, a
   config directory the settings sheet edits under drift lock. MECHANISM: a
   `#+GLANCE_KEYS:` block (or `keys.org` table) in the config layer — `SEQ →
   command` lines, read at serve time, overriding `keyBindings` rows by
   command name; the settings sheet grows a KEYS panel listing the merged map
   (the palette from #1 doubles as its browser). COST: medium — the read, the
   merge, the panel — but it retires the AZERTY/Dvorak and Russian-layout
   consequences the keymap bullet documents as known losses.

4. **Zen mode — the table alone.** VSCode hides all chrome around the
   editor. Glance's chrome is already two strips (log, key line); a toggle
   that collapses both (`Z`, or a settings row) leaves the table full-bleed —
   reading mode for a wall display or a phone. COST: trivial (two
   `display:none`s and a preference under `glance-zen`); the echo pill stays,
   being the one voice. Worth doing when a touch/phone pass happens anyway.

5. **Peek — a subtree glanced without the sheet.** VSCode's peek shows a
   definition inline without leaving the file. Glance's RET materializes the
   FULL sheet; a peek (`v`, dired's own view key) would raise a read-only
   `.pop-band` of the subtree's first N lines — cheaper than the sheet when
   the question is "what is in this row" and the answer is one glance.
   COST: small-medium (one popup, `GET /headline` already serves the text) —
   but it is a NEW surface, and the sheet is one keypress; do it only if the
   sheet's open cost ever starts to bite on the ports.

## Already ours, under other names

- **Quick Open** (`Ctrl+P` fuzzy files) — the filter palette with the title
  completion tier IS fuzzy-jump-to-row; the server narrows store-wide.
- **Breadcrumbs** — the sheet's `mwhere` trail and the drill crumbs.
- **Multi-cursor** — marks + bulk commands are the row-model's spelling.
- **Snippets** — capture templates (`%^{PROMPT}`, `%?`, `%U`).
- **Outline view** — the structured document pane is the outline, editable.
- **Timeline** — the logbook strip shows the entry's history the file keeps.
- **Sticky table header** — the renderer ships it (z 1).

## Rejected on the way

- **Minimap** — chrome, mouse-first, and a 13k-row table's minimap is noise.
- **CodeLens / inline hints** — annotations INSIDE content violate "every
  byte on screen exactly once"; the echo and the log are the voices.
- **Extensions marketplace** — the extension surface is org-glance's elisp
  side; a JS plugin API is a framework by the back door.
- **Editor groups / splits / floating windows** — one table, one sheet; the
  window manager owns windows (the native pane already follows that rule).
- **Settings Sync** — the config lives in org files; the user's own sync is
  the sync.
- **Search editor** — the query's answer already IS a persistent, addressable
  view (the URL carries `q`).
- **Notebooks, tasks, testing UI, remote dev, workspace trust** — different
  product.
- **Recently-visited ring** (`Ctrl+Tab`) — org's mark-ring idiom would fit
  (`C-u SPC`), but the drill crumbs already cover the common back-out; hold
  until a reader asks.

## First three

1 (command palette) is the discoverability multiplier and nearly free — the
blob is the list. 2 (sticky headings) is a CSS afternoon that pays on every
long subtree. 3 (rebinding) is the roadmap item the doctrine already owes;
1's palette is half its UI.

## Sources

- https://code.visualstudio.com/docs/getstarted/userinterface
- https://code.visualstudio.com/docs/editing/tips-and-tricks
- https://hackr.io/blog/best-vscode-shortcuts
- https://www.howtogeek.com/i-thought-i-knew-vs-code-but-these-features-proved-me-wrong/
