# Issue Tracker

Where issues live for this repo, and how the engineering skills should read and
write them.

**This repo does not use GitHub Issues.** The remote is GitHub
(`rails-to-cosmos/glance`), but the `gh` CLI is not installed and the issue
tracker is unused. Do not call `gh`.

Work is tracked in two places, and which one a piece of work belongs in is
decided by whether it needs a design record.

## The queue: `fixme.org`

A numbered org list at the repo root. Status is org's own checkbox, and the file
says so at the top:

```org
# Status is org's own checkbox: [ ] pending, [-] in progress, [X] done.
# SPC toggles one in glance's own material view.

17. [ ] the thing to do, stated as the behaviour a reader would notice
    1. [X] a sub-step that already landed, with what it landed as
    2. [ ] the sub-step still owed
```

Rules:

- **Append at the next number.** Numbers are never reused or renumbered; a
  reader refers to "fixme 11" and that has to keep meaning one thing.
- **Sub-items are the plan**, and they carry their own checkbox. A partly-done
  item is `[-]` with its sub-items telling which half landed.
- **State the user-visible behaviour**, not the patch. "Marking a repeating row
  DONE stops the repeat dead" beats "fix `repeatOn`".
- The file is read and toggled **in glance itself** — this project tracks its
  work in the tool it is building, so keep it valid org.

To add an issue: read the file, take the next integer, append. There is no CLI.

## The design record: `docs/proposals/`

Anything that needs a decision written down — a new mechanism, a rule change, a
trade-off someone will ask about later — earns a proposal.

```
docs/proposals/YYYY-MM-DD-<slug>.<status>.md
```

`<status>` is one of `proposed`, `partial`, `done`, `expired`, `draft`.

The header repeats both facts:

```markdown
# Proposal — one line saying what it does

**Status:** proposed · **Date:** 2026-08-14 · **Origin:** where it came from
```

**The name and the header are compared by a test.** `TestSelfContained.hs`
("every proposal's name is the date and status it declares") lists the directory
and asserts each filename's date and status against the file's own `**Date:**`
and `**Status:**` lines. So:

- the DATE never changes after the file is written — that is what makes it safe
  in a name;
- a status change is a `git mv` **and** a header edit, together, or `cabal test`
  goes red;
- a document cannot escape the check by being misnamed, because the sweep reads
  the directory rather than a prefix.

Emacs sidecars (`#name#`, `.#name`) are skipped by that sweep and are not
proposals.

## Which one

- A bug, a small behaviour change, a chore → **`fixme.org`**.
- A mechanism, a contract between the two repos, anything with a trade-off worth
  defending later → **a proposal**, and usually a `fixme.org` line pointing at
  it.

## PRs as a request surface

**Off.** Branch protection exists on `master` but is bypassed; work lands
directly. Do not treat pull requests as an inbox.
