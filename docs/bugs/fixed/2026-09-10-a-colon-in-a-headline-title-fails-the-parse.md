# Bug — a colon in a headline title fails the whole file's parse

**Status:** fixed · **Reported:** 2026-09-10 (boot: *"doctor: 6 files failed to
parse"*) · **Surface:** `Data.Org.Parser` (`tagsP`)

## Symptom

Six files in `~/sync/views` fail to parse, so their headlines — 54 of them,
counting subtrees — never enter the store; `doctor` only counts the files, it
cannot recover them. All six are org-glance blobs whose headline title carries a
colon that is not part of the trailing tag block:

- four use `::` as a separator — org-glance's overview convention
  (`.emacs.d :: emacs config`, `Task Management :: Board :: Tasks`, …)
- two carry a `:)` smiley (`Leave everything as it is :)`, `… lesson :)`)

The parser reports `unexpected ':'` / `unexpected ')'` at that interior colon.

## Root cause

Tags are read by

```haskell
tagsP = hspace1 *> (char ':' *> many tag)   -- many = ZERO or more
        where tag = takeWhile1P isTagChar <* char ':'
```

`many tag` accepts **zero** tags, so a mid-title ` :` matches an *empty* tag
block: it consumes the space and the colon and succeeds with no tags. The rest of
the line (`: emacs config … :Bookmark:`, or the `)`) is then left over, and the
headline line fails on the stray character. Org parsing is all-or-nothing per
file, so the whole file is rejected. These titles are valid org — emacs and
org-glance accept them; only the trailing `:tag:` is a tag.

## Reproduce

`TestParser`: "A '::' in the title is not a tag" (`* Foo :: Bar :c:` → title
`Foo :: Bar`, tag `c`) and "A ':)' smiley in the title is not a tag" — both red
before the fix. End to end: `glance doctor ~/sync/views` went 6 → 0 parse
failures and 7652 → 7706 headlines.

Evidence: `glance doctor ~/sync/views` names the six files with the column of the
interior colon; `Parser.hs` `tagsP`.

## Fix

`tagsP` requires **at least one** tag — `char ':' *> some tag`. A colon with no
tag name after it is then title text, `try tagsP` backtracks cleanly, and a real
trailing `:tag:` still parses.
