# Proposal — undo in the material doc

**Status:** proposed · **Date:** 2026-09-02 · **Origin:** user — *"prepare a
proposal for 'undo', or even re-use 'undo-tree' as in Emacs, for the material
doc."* Raised as the table-mount edits (cell/row/column writes) made the pane a
place a reader edits freely, with no way back from a mistaken write.

## The law in one line

An UNDO is a FORWARD WRITE of a prior state through the one door every edit
takes — drift-locked, watched, reloaded — so undo needs no second write engine
and no second source of truth; the only new thing is the pane REMEMBERING the
states it passed through, and a key to walk them.

## Today

The pane edits by writing. A cell edit, a paragraph, a planning value, a row or
column add — each composes a new body and leaves through `composed` →
`docBody` → `commitDoc` → `POST /command`, drift-locked by a SHA-256 of the
exact bytes it read (`Edit.hs`, the digest; `Routes.hs`, the pin). The server
writes the file, the watch reloads it, and the pane gets a fresh `fill`
(`applyFill`, `Doc.elm`). There is no way back: once a write lands, the bytes it
replaced are gone from the pane, kept only in the file's own history — which the
material doc, holding no org buffer, cannot reach. Emacs' `undo`/`undo-tree` is
BUFFER-LOCAL; this pane is not a buffer, it is a view onto a file the server
owns, so there is nothing buffer-local to walk.

Three facts the design leans on, and they are the reason it is small:

1. **Every edit already has a BEFORE and an AFTER.** The write pins the
   before-bytes' digest and carries the after-body; the pane holds both across
   the flush. Undo needs only to keep the BEFORE around one step longer.
2. **The write is one door, and it is idempotent about direction.** Writing the
   before-body back is a WRITE like any other — drift-locked against the
   current bytes — so undo is not a new mechanism, it is `commitDoc` pointed
   backward. Undo is therefore itself undoable: redo is the same door, forward.
3. **The pane is scoped to ONE subtree.** Every edit is within the materialized
   headline (the narrowing invariant, `docs/invariants.md`), so the undo history
   is per-pane and its states are whole subtree bodies — small, self-contained,
   and never entangled with edits to other files.

## The design: a stack of subtree states the pane walks

- **The unit is a COMMITTED WRITE.** Each successful `commitDoc` — a cell edit,
  a paragraph, an add, a delete, a rename, a planning value — is one undoable
  step. The draft/ESC dance and refusals write nothing and so are no step.
- **The state is the subtree's WRITE CARGO**: the `body` and the `header`
  (planning + properties) the flush carried — exactly what `cargoJSON` /
  `stateJSON` already build (`Doc.elm`). A snapshot is that pair plus the digest
  the write was pinned to.
- **The history is a POINTER INTO A LIST**, held by the shell beside the pane
  (`20-sheet.js`), the way `undo-tree` holds a position in its tree:
  - Before a write commits, push the state it is LEAVING (the before-cargo) onto
    the list at the pointer, dropping anything past the pointer (a new edit off
    a mid-history point abandons the redo tail — the linear case).
  - `undo` writes the state one back and steps the pointer back; `redo` writes
    the state one forward and steps forward. Both go through the ordinary door.
  - The list lives only while the pane is open, and resets on close — buffer
    undo's own rule (`kill-buffer` forgets).
- **Undo is a real write.** It re-touches the file (mtime moves, the watch
  nudges, the external ledger records it) — the honest cost of "the file is the
  truth." An undo that changed nothing (the state already stands) writes
  nothing and says so.

## undo-tree, weighed

Emacs `undo-tree` keeps a TREE, not a stack: an edit after an undo BRANCHES
rather than discarding the redo path, and the reader can walk to any past state.
It buys back the states a linear undo throws away.

**The recommendation is LINEAR FIRST, the tree as a named phase 2.** The pane's
session is short and its edits few; a linear undo/redo covers the mistaken-write
case the request is really about, at a fraction of the surface. The tree is a
clean superset — the same snapshots, a parent pointer instead of a list index,
and a visualizer key (`undo-tree-visualize`) — so nothing in phase 1 blocks it,
and the snapshot format is chosen so the tree can adopt it unchanged: each
snapshot already carries its own before-digest, which is the edge a tree walks.

## Where the history lives — the pane, not the server

- **The shell (client).** RECOMMENDED. The pane already mirrors every write's
  cargo; keeping a list of them is a few fields beside `editing`. It matches the
  scope (one open pane, one subtree) and the lifetime (the session), and it
  needs NO server change — undo is `commitDoc` with a remembered body.
- **The server ledger** (`meta/EXTERNAL.jsonl`, `External.hs`). Rejected for the
  first cut: the ledger is APPEND-ONLY, best-effort and DERIVED
  (`docs/invariants.md`), a record for peers, not a rollback engine; replaying
  its inverse would make it authoritative for a state it was built to only
  observe, and would couple undo to a file the daemon writes for others. It is
  the right home only if undo must PERSIST across sessions — a separate, larger
  question named in *Out*.
- **The org file's own history** (git, Emacs' `.~undo~`). Rejected: the page
  holds no org tooling and must not grow any; the file is the server's.

## The keys

Org and Emacs spell undo `C-/` (and `C-x u`), redo `C-?` / `M-_`; `undo-tree`
adds `C-x u` for the visualizer. In the pane:

- `C-/` — undo, `C-?` — redo, claimed the way the doc's other `C-` keys are
  (`20-sheet.js` keydown), each echoing what it walked back to ("undo → cell
  Owner" / "nothing to undo"). A bare `u` is the dired UNFLAG and stays that.
- The visualizer (phase 2) hangs off `C-x u` as a popup surface, the shape the
  links/tags popups already wear.

## The drift seam — the one real subtlety

The pane's snapshots were taken against digests that the FILE may have moved
past — a `C-c '` in Emacs, another client, the daemon. The one door already
answers this: an undo write is pinned to the CURRENT bytes, so:

- If the file has NOT moved since the last pane write, undo lands cleanly.
- If it HAS, the undo write DRIFTS (409) exactly as any stale write would; the
  pane reloads the fresh subtree (`READ reloads on drift`, `docs/invariants.md`)
  and the history is INVALIDATED — the redo tail and any snapshot pinned to a
  vanished digest are dropped, because they describe a document that no longer
  exists. This is Emacs' own rule: an external change to a buffer's file breaks
  the undo chain. The proposal INHERITS the invariant rather than adding a wall.

So undo never writes over an edit the reader did not make; the worst case is
"undo refused — the file moved under you," a reload, and an empty history.

## Out, named

- **Persistence across sessions.** A closed-and-reopened pane forgets, as a
  killed buffer does. Persistent per-file undo is a server-history feature (the
  ledger, or a shadow), out of this pane-local proposal.
- **Undo across subtrees / the whole file.** The pane is one subtree; undo is
  its own. A file-wide undo is a different surface (the query table has no edit
  to undo).
- **Coalescing keystrokes.** Each COMMITTED write is one step; there is no
  per-character undo, because the pane has no per-character write — a cell edit
  is one commit, not one write per key. This is coarser than Emacs and correct
  for a pane that writes whole lines.
- **Undoing a materialize / a capture** (a write that CREATED the subtree): the
  first snapshot is the state the pane opened on, so undo stops at "as opened,"
  never deleting the entry out from under itself.

## Alternatives considered

- **A client-side model that never writes on undo, only re-renders.** Rejected:
  the file would then disagree with the pane, and the next real write would
  drift or clobber. The file is the truth; undo must write.
- **Snapshotting the ROWS rather than the cargo.** Rejected: rows are derived
  and positional; the cargo (`body`/`header`) is exactly what a write already
  carries and what a fill already rebuilds rows from, so it round-trips with no
  new code.
- **A dedicated `POST /undo` route.** Rejected: it would be a second write door
  for the one thing the existing door already does, against the one-door
  invariant; undo is `POST /command` with a remembered body.

## Oracle

- TestServe: a cargo written, then an `undo` posts the PRIOR body drift-locked
  against the current digest, byte-for-byte the pre-edit subtree; a `redo` posts
  it forward; a new edit off a mid-history point drops the redo tail; an undo
  whose file moved underneath takes the 409 and empties the history.
- Browser: edit a cell, `C-/` returns it to its old value and re-selects it (the
  cell-selection-after-write fix already keeps point); `C-?` re-applies; `C-/`
  at the opening state says "nothing to undo"; an add then `C-/` removes the
  row.

## LOC, roughly

+60 glue (the snapshot list beside `editing`, the two keys, the push-before-
commit hook), +0 Elm (the cargo already exists; undo re-sends a body through the
existing `edit`/`insert` door), +0 server (the write is the write). Phase 2
(undo-tree + visualizer): +120 glue and one popup surface, no new write path.

## Phases

1. **Linear undo/redo** — the snapshot list, `C-/`/`C-?`, the drift-invalidation.
   Shippable and the whole of the mistaken-write cure.
2. **undo-tree** — a parent pointer instead of a list index, branching on a new
   edit off a mid-history point, and a `C-x u` visualizer popup.
3. **Persistence**, only if wanted, and only then the ledger/shadow question.

Inert until reviewed.
