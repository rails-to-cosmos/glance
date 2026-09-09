# Spike — a git sync control for the served dir

**Date:** 2026-09-10 · Brainstorm, not wired into the main code.

**Open `index.html`.** glance's actual main-page omnibox — the filter box takes
the bar's width — with the branch (`⎇ master`) and a **sync** control at its
end. Flip the git state with the row of buttons (clean / behind / ahead / dirty
/ diverged) and watch the glyph, the count and the colour follow, the way
magit's mode-line does. Click the sync glyph to run the state's one obvious
action — the git commands echo in the log below, no panel. Toggle **auto-sync**
to watch the second model run.

## The idea

When the served directory is a git work tree, glance shows a one-glance sync
control on the main page. Git only for now — no other VCS.

## What the glyph says (magit-inspired)

| State | Glyph | Colour | One click does |
|-------|-------|--------|----------------|
| clean, up to date | `✓` | muted | fetch |
| behind upstream | `↓ N` | blue | pull |
| ahead of upstream | `↑ N` | green | push |
| dirty (uncommitted) | `● N` | amber | commit + push |
| diverged | `↕ N↓ M↑` | red | pull --rebase, then push |
| detached / no upstream | `⚠` | grey | logs a note — nothing safe to one-click |

The count is the number the action moves: commits to pull/push, or files dirty.
Every state but detached has one safe next step, so the click just does it and
echoes the git commands in the log — no panel to open. A full magit-style status
view (the staged / unstaged / untracked file list) is a real-build concern,
listed below.

## Two models

**A — manual one-button sync.** The glyph reflects state; a click does the
obvious next step (above) and logs it. Detached, with no safe step, only logs a
note. This is the safe default: nothing leaves the machine until the reader asks.

**B — auto-sync on every atomic change.** glance already writes by one atomic
span-splice per edit. After each, stage everything, commit with a terse
auto-message, and push — so the repo is *always* in sync and the glyph sits on
`✓`. The showcase's log shows the `edit → stage → commit → push` ticks.

- **All changes staged.** "Sync after each change" means `git add -A` before the
  commit, so a file the reader touched outside glance rides the same commit.
- **The offline hole (can't be closed here).** A change made in a plain editor
  while glance is not running, or on another machine, bypasses this entirely — so
  "always in sync" holds only for glance-mediated edits. The control must still
  show *behind* and offer a pull when upstream moved under it. Auto-sync removes
  the *ahead/dirty* half of the problem, never the *behind* half.

## What a real build would need (not spiked)

- Backend: `GET /git` → `{ repo, branch, upstream, ahead, behind, staged,
  unstaged, untracked }` (shelling `git` in `soDir`); `POST /git/sync` → the
  action for the current state. Loopback only, as the rest of the server is.
- Frontend: the glyph at the end of the omnibox bar, polling `/git` (and on
  focus / after a write), plus a full magit-style status view for the details.
- Model B: hook the write path (`replaceSpans`) to fire the stage/commit/push,
  debounced, off the request path so a slow push never blocks an edit.
- Security: push is outward-facing. A visible confirm for B's first push, or a
  per-repo opt-in, keeps a surprise auto-publish from happening.
