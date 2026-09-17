# Proposal — page routes are not overlay surfaces

**Status:** done · **Date:** 2026-09-17 · **Origin:** flat settings route follow-up

## Pattern

Settings now replaces the main table, but remains a member of the modal
`SURFACES` registry (`frontend/glue/70-shell.js:17-41`) and participates in the
material sheet's `activeSheet`/`leaveSheet` ladder
(`frontend/glue/20-sheet.js:1597-1663`). The route conversion also introduced
a cycle: settings edits the git-control DOM to render its address
(`frontend/glue/50-settings.js:37-49`), while the git control reads `settings`
and calls `leaveSheet` (`frontend/glue/80-git.js:20-37`).

The shared save ladder is valuable. The shared navigation category is false:
a full-page table route has selection restore, socket ownership and a page
address; an overlay has stacking, dismissal and focus containment.

## Proposed change

Introduce two small contracts:

```js
/** @typedef {{ name:string,
 * enter():void|Promise<void>, leave():void|Promise<void> }} PageRoute */
/** @typedef {{ dirty():boolean, save():Promise<boolean>,
 * discard():void }} SaveSession */
```

A `Pages` coordinator owns the active route, main mount replacement, URL,
selection restore, socket stop/start and breadcrumb model. Settings implements
both `PageRoute` and `SaveSession`. The material document remains an overlay
that implements `SaveSession`. `saveSheet` can be renamed `saveSession` without
making the two surfaces share navigation.

`PageControl` receives a page-address reader and an `onPageBack` callback. It
does not read settings state or call a sheet closer. `GitControl` is a sibling
that contributes repository location and status only when `/git` reports one.

`SURFACES` then contains overlays only: mint, prompt, refer, links, tags and
the material document. A later full-page table becomes one `PageRoute` entry
and does not enter modal key scope.

## LOC estimate

Approximately +25/−35 immediately. The main gain is the next page variant:
one route object and its unique table model, instead of registrations in the
overlay ladder, generic sheet navigation and git-control internals.

## Risk

Medium. URL restoration, dirty settings writes, WebSocket restart and prior
row restoration are lifecycle-sensitive. Existing settings, git breadcrumb
and browser remount cases cover each boundary. No server or on-disk format
changes.

## Existing precedent

`SURFACES` already centralizes overlay lifecycle and exclusivity
(`frontend/glue/70-shell.js:17-73`). The new `Pages` registry applies the same
data-owned lifecycle to a distinct kind of surface. Keep the Haskell popup
registry limited to actual popup chrome.

## Outcome

`Pages` now owns the active route, URL page value, main-table replacement,
socket suspension, selection restoration and breadcrumb address. Settings is
the first `PageRoute`; it no longer appears in `SURFACES` and no longer keeps a
second boolean copy of route state.

The write ladder is now named for its actual abstraction: `SaveSession` and
`saveSession`/`leaveSession` serve both the material overlay and settings page.
The keymap gained a `session` scope for `C-x C-s`, so settings receives the one
shared save command without inheriting material-document commands from modal
scope.

The shell now renders `PageControl` from `Pages.address()` and owns its back
action. `GitControl` is a separate optional sibling: it renders only repository
location and synchronization state, and stays hidden when `/git` says the
directory is not a repository.
