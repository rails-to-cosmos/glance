# Configuration

Everything a tree configures lives in org files under
`<root>/.org-glance/config/` — plain pragmas a person can edit in Emacs or
through the settings sheet (`,`), and the walk reads none of them as rows.
The README's Config section is the crib; this page is the whole law.

## The layers

```
.org-glance/config/
  system.org        the tree-wide layer
  tags/TAG.org      one layer per tag: its cycle, its capture template
```

```org
# system.org
#+TODO: TODO STARTED | DONE CANCELLED
#+GLANCE_DEFAULT_FILTER: state:*active* -tag:someday
#+GLANCE_STATE_COLORS: light TODO=#7B1FA2 DONE=#00695C
#+GLANCE_STATE_COLORS: dark  TODO=#B584D9 DONE=#2BB5A0
```

A tag layer is the same shape plus a capture template — its first `*`
heading to the end of the file ([capture.md](capture.md)); everything above
the heading is the pragma region, so the two never overlap.

## Keywords: recognition unions, classification is widest-scope

- **Recognition** unions every layer: a keyword any layer names is a keyword
  everywhere — a row wearing it is stated, whatever file it sits in.
- **Classification** (is it active or done-like?) is decided at the widest
  scope that names it: built-in > system > tags > file. A file redeclaring
  `TODO` cannot make it done-like; a tag layer cannot flip what system
  settled. Each tag keeps its **first** config; the union keeps them all.
- A tag with no layer has no `#+TODO:` cycle of its own — worth minting one
  (the settings sheet is the one place that creates a layer file) before
  giving its rows custom states.

## Saved views

Three names, each one pragma, each with a built-in fallback
([query.md](query.md) for what the queries mean):

| view | pragma | built-in |
| --- | --- | --- |
| `default` | `#+GLANCE_DEFAULT_FILTER` | `state:*active*` |
| `agenda` | `#+GLANCE_AGENDA_FILTER` | `state:*active* -planned:*empty* sort:scheduled` |
| `archive` | `#+GLANCE_ARCHIVE_FILTER` | `tag:archive` |

`g` / `A` apply default and agenda; `P` pins the table's current query into
a saved view — written back to the layer as the pragma line. Within one
file the **last** pragma line wins; across layers the **first system layer**
that names a view wins. The default view is also applied at boot when the
address bar carries no query, and its config values are spoken into the log
panel as the walk lands.

### A today agenda

The agenda's built-in serves every dated row. A tree that wants the day's work
writes the comparison into its own pragma:

```
#+GLANCE_AGENDA_FILTER: state:*active* planned:<=today sort:scheduled
```

`<=today` carries the overdue along with today's rows, which is what an
agenda is usually asked for; `planned:today..today` is the strict reading,
today's rows alone. Both forms are
[query.md](query.md#comparisons-on-the-date-keys)'s.

### A lookahead agenda

A tree that wants the month ahead rather than the day shifts the far end:

```
#+GLANCE_AGENDA_FILTER: state:*active* planned:today..today+30d sort:scheduled
```

`today+30d` is a date wearing a shift — `d`, `w`, `m`, `y` are the units, and
the sum is worked out once per request, so `A` reads the pragma as the day it is
pressed. Swap `30d` for `7d` or `1m` and the window moves with it;
`planned:today-7d..today+30d` is the same lookahead with last week's
stragglers kept, and `planned:<=today+30d` keeps every overdue row instead.
A pragma still spelling `*today*` is read unchanged — it is the old spelling of
the same word ([query.md](query.md#today-is-a-date)).
The shift's whole law is
[query.md](query.md#a-date-can-be-shifted)'s.

## State hues

```
#+GLANCE_STATE_COLORS: THEME KEYWORD=HUE KEYWORD=HUE …
```

One line per theme is the shape; **every** line is read, and a keyword named
twice in a theme takes its **last** spelling — so appending a line overrides
without editing history. Shape alone is validated: an unknown theme declares
tokens nothing reads, a non-colour is a value CSS ignores — both the
author's business. The settings sheet edits hues per theme with a picker,
and unlike the views, colors gather across **every** layer.

## What the page remembers outside the tree

Browser-local, per profile, never written to the tree: the theme
(`glance-theme`: `auto`, or a theme id, stamped before first paint), the
reading line the document pane rests point's row on (`glance-reading-line`: a
whole percent, banded 20–90, 60 by default) and the log panel's height.
Everything else — views, cycles, hues, templates — is the tree's, so every
device reads the same configuration.

## Untagged captures

`<root>/inbox.org`, always — the one config-adjacent path that is a row
source. See [capture.md](capture.md).
