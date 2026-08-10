# Proposal — one traversal policy, not two

**Status:** proposed · **Date:** 2026-08-10 · **Found by:** /generalizer over
`e1ba099..6412f4a` (the `delete` command)

## Pattern

`Data.Org.Walk` states a symlink policy as an invariant and enforces it in one
place (`Walk.hs:179-190`): ONE `lstat` classifies an entry, a symlinked
DIRECTORY is never followed — "link loops, and one tree twice" — and a symlink
pays a second `getFileStatus` only where the answer could change what is
collected.

`Data.Org.Trash.filesUnder` (`Trash.hs:98-105`) is a second recursive
traversal, written a week later, that honours none of it:

```haskell
one path = do
  isDir <- doesDirectoryExist path        -- FOLLOWS a symlink
  if isDir then filesUnder path else pure [path]
```

`doesDirectoryExist` stats the TARGET, so a symlinked directory inside a blob is
descended into, its contents are gzipped into the trash under the blob's own
mirror, and `removeDirectoryRecursive` then removes the LINK rather than what
was copied. The bytes are duplicated into the trash and the original tree keeps
them. A blob carrying a symlink is unusual; the asymmetry is the finding, not
the frequency.

`Data.Org.Config` has a third `listDirectory` (`Config.hs`), but it is one level
and takes no policy — no finding there.

## Files

- `src/Data/Org/Trash.hs:98-105` — `filesUnder`
- `src/Data/Org/Walk.hs:174-195` — `visit`, where the policy lives
- CLAUDE.md, Walk: "Symlinked directories are never followed"

## Proposed change

The two traversals want different ANSWERS — Walk collects org documents under a
denylist, `filesUnder` collects every regular file under one directory — so
they should not become one function. What they share is the CLASSIFICATION, and
that is what to lift:

```haskell
-- Data.Org.Walk, exported
-- | What one directory entry IS, by a single `lstat' that never follows: the
-- rule every traversal in this program reads an entry with.
data Entry = Dir | Regular | Linked
entryOf :: FilePath -> IO Entry
```

`visit` reads its answer from `entryOf` (it already computes exactly this), and
`filesUnder` becomes:

```haskell
one path = entryOf path >>= \case
  Dir     -> filesUnder path
  Regular -> pure [path]
  Linked  -> pure [path]   -- kept as the LINK it is, never descended
```

A trashed symlink is then kept as a file (its own bytes, which is the link
text) and the target is left alone — the same answer Walk gives.

## LOC estimate

- Added: ~10 (the type, `entryOf`, its haddock).
- Removed immediately: ~4, and the second policy.
- Saved per future variant: a fourth traversal reads the rule instead of
  guessing it. There is no fourth today, which is why this is ranked last.

## Risk

`visit` is the walk's hot path and CLAUDE.md is explicit that it costs ONE
`lstat` per entry, with a second `getFileStatus` only where the answer could
change what is collected. A refactor that adds a stat to that path is a
regression, not a cleanup — `entryOf` must return the same information from the
same single call, and `visit`'s symlink branch must keep its conditional second
stat rather than folding it in.

## Existing precedent

The repo's own reason for one `isDerived` serving both the walk and the watch:
"so a file the store never loaded cannot arrive by inotify". One policy, two
readers, is the shape already argued for there.
