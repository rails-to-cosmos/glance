# Bug — a multi-line CSS comment drops the following rule

**Status:** fixed · **Reported:** 2026-09-10 (found while debugging the git
control's CSS) · **Surface:** `Glance.Web.Page.Style` (`styleBody`'s comment strip)

## Symptom

A rule in `assets/page.css` whose preceding comment spans more than one line is
**silently dropped** from the served stylesheet — its style never applies.
`page.css:648-650` does this to `#mdoc .de.d-drawer > .fold:hover`: the drawer
fold's hover colour is dead in the served page.

## Root cause

`styleBody` strips comments line by line, dropping only lines that *contain* `/*`:

```haskell
filter (\l -> not (T.null (T.strip l)) && not ("/*" `T.isInfixOf` l))
```

A comment written across lines —

```css
/* The sign lifts with the spine at point, since both read `--spine'; no rule
   of its own is needed. */
#mdoc .de.d-drawer > .fold:hover { … }
```

— keeps its continuation line `of its own is needed. */` (no `/*`), which lands
in the served CSS in front of the next rule. As a selector prefix it makes
`of its own is needed. */ #mdoc … .fold:hover` invalid, so the browser drops the
whole rule. A multi-line comment *inside* a block instead leaves a bad
declaration, which the browser ignores harmlessly — so only comments before a
rule bite, which is why the failures looked random.

## Reproduce

`TestServe` — "no CSS comment leaks into the served stylesheet": the served
`<style>` must carry no `*/`, and `ruleLine "#mdoc .de.d-drawer > .fold:hover"`
must resolve. Both fail before the fix.

Evidence: `curl / | grep 'of its own is needed'` prints the leaked bytes;
`Style.hs` `styleBody`'s `filter`.

## Fix

Strip `/* … */` spans as spans (multi-line included), not by line — `page.css`
carries no `/*` inside a string, so a straight scan is safe. Multi-line comments
now work, and the repo's per-line-comment convention is no longer load-bearing.
