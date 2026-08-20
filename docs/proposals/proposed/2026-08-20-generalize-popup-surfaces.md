# Proposal — the popup surface trail joins the one list

**Status:** proposed · **Date:** 2026-08-20 · **Origin:** /generalizer — the
future-variant sweep measured the last two surfaces landing at 18 files
(`d11f1ea`, mint) and 24 files (`ba4f8db`, refer).

## Pattern

`Glance.Web.Page.Popups` already collapsed five readers (veil, `.on` rule, box
sizing, stale wash, tier sweep) onto one list — and the rest of a surface's
trail is still hand-written per member, in four places that do not read it:

- `src-web/Glance/Web/Page.hs:24-121` — each popup's markup body, spelled out
  (`popupFrame`/`tableFrame` cover only the wrapper);
- `frontend/glue/70-shell.js:17-42` — the runtime `SURFACES` record
  (momentary/up/off/edit/shut/narrow/wide/open/rowed/panel);
- `src-web/Glance/Web/Page/Style.hs:100,132,347,352,355,390,421-424` — bespoke
  rules naming surfaces one by one;
- `test/TestServe.hs:5067-5075` — the veiled selector list as one string
  literal (this one is at least enforcing).

`AGENTS.hs`'s `surfaces` mirror sat two members behind (`mint`, `refer`
missing) until this batch — the drift the unenforced copy invites.

## Proposed change

Three steps, each separately shippable:

1. **Enforce the mirror.** A `TestSpec` case comparing `Spec.surfaces` names
   and flags against `Popups.hs`'s list, and a `TestServe` needle comparing
   the shell's `SURFACES` names against the same — the textual harness
   `objectKeys` already used for `HANDLERS` (`TestServe.hs:9241-9288`) is the
   precedent.
2. **Widen the record.** `Popups.hs`'s per-surface row grows the flags the
   shell spells (`rowed`, `edit`, `narrow`, `panel` are already in
   `AGENTS.hs`'s `Surface`); `Page.hs` derives each popup's *frame* (id, tier
   class, `popupFrame` vs `tableFrame`) from the row, keeping only the body
   markup bespoke.
3. **Style off the list.** The per-surface selector splices in `Style.hs`
   (`:347-355` and kin) generate from the same rows, the way the veil already
   does.

## LOC estimate

+40 (wider record, two comparison tests, frame derivation) / −60 immediate
(hand-written frames and selector lists). Next surface: ~4 files (row, body
markup, shell behaviour, drive) instead of 18–24.

## Risk

Markup-shape churn in `Page.hs` moves TestServe needles; the veiled-selector
literal is deliberately independent and stays. No wire or store impact.

## Existing precedent

`Popups.hs` itself — the proposal that created it is cited in its header; this
finishes the walk it started.
